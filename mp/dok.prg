#include "dm_form.ch"
#include "inkey.ch"
#include "getexitm.ch"
//#include "dbinfo.ch"

#define I hb_UTF8ToStr("│")

#ifndef A_MKNK
  //#define A_MKNK(x) eval({|y|mknk:=min(5,max(mknk,len(ltrim(str(y))))),pad(str(y,mknk),5)},x)
  #define A_MKNK(x) str(x,4)+' '
#endif
#ifdef A_KASA
request __msave,__mrestore,__dbzap
#endif
#ifdef A_ALTCEN
 #define A_CENSPEC
#endif
#ifdef A_JMTOT
#define D_JM jm
#else
#endif

#ifdef A_DIETA
  #ifndef A_MAGDI
    #define A_MAGDI " 1"
  #endif
#endif


#define WAPIC "@KE # ### ###.##"
#ifdef A_OLZBY
#define WANAZ "WAGA kg"
#else
#define WANAZ "Wartość"
#endif
#ifdef A_SZTUKI
#define ILDEC 0
#define ILPIC "### ### ###"
#else
#define ILDEC 3
#define ILPIC "### ###.###"
#endif


#ifdef A_MM

#ifdef A_SUBDOK
#define KEY_PAR left(dok,2)
#else
#define KEY_PAR dok
#endif
#define KEY_DOK smb_dow

#else

#ifdef A_SUBDOK
#define KEY_PAR mag_biez+left(dok,2)
#else
#define KEY_PAR mag_biez+dok
#endif

#define KEY_DOK nr_mag+smb_dow

#endif

#define dok_p_r dok_par[MAG_POZ,r,1]
#define dok_zew dok_par[MAG_POZ,r,2]
#define dok_kon dok_par[MAG_POZ,r,3]
#define dok_war dok_par[MAG_POZ,r,4]
#define dok_lpm dok_par[MAG_POZ,r,5]
#define dok_kop dok_par[MAG_POZ,r,6]
#define dok_naz dokumenty[MAG_POZ,r]
#define dok_ew  dok_par[MAG_POZ,r,8]
#define dok_fk  dok_par[MAG_POZ,r,A_FK]
#define dok_kpr dok_par[MAG_POZ,r,A_KPR]
#ifdef A_DF
#define dok_df  dok_par[MAG_POZ,r,A_DF]
#else
#ifdef A_CENVAT
#define dok_df  .t.
#else
#define dok_df  .f.
#endif
#endif
#define dok_co  dok_par[MAG_POZ,r,A_DOKCOMP]
#define dok_wal dok_par[MAG_POZ,r,A_WALUTA]
#ifdef A_TPD
#define dok_tpd if(FIRMY->(EOF()),dok_par[MAG_POZ,r,9],FIRMY->tp_dni)
#else
#define dok_tpd dok_par[MAG_POZ,r,9]
#endif
//#define dok_kh  (dok_zew#"W" .and. ""#dok_kon .and. dok_naz#"K")
#define dok_kh  (dok_zew#"W" .and. ""#dok_kon )

MEMVAR r,mag_biez,mag_poz,magazyny,adres_mag,is_spec,operator,dok_par,dokumenty,;
       przegl,dok,zamowienie,stanowis,stary_rok,nowydm,da,dd,hlink,;
       d_o,n_f,nk,changed,nim,nz,il,gil,lam,itot,gtot,posproc,canopen,miar_opcja,;
       ce,ck,wa,chg_cen,dok_w1,nowystan,dflag,darr,dpos,dpush,kk,sk,kont_kos,;
       DZIALY,dzial,mater,kont,kos,zap,zac,nrc,uw,cz,tp,wz,rodz_sprzed,;
       sp,st,pv,vat,npr,pm,zaplac,dazapl,ppos,ppush,parr,pflag,avat,stawki,;
       stawkizby,chgpos,DOK_ZB,STANO,dok_di,dok_ksef,kh,nazwis,posilki,diety,path_zb,defa,;
       dv,nkp,mknk,n_ksef,xml_ksef
field  data,smb_dow,nr_dowodu,pozycja,nr_zlec,ilosc,index,numer_kol,;
       DATA_PRZY,data_dost,dost_odb,kontrahent,nr_faktury,nr_mag,kto_pisal,jm,nazwa,stan,;
       data_zmian,opis_koszt,uwagi,data_roz,gram,rodz_opak,nr_rys,konto,;
       jm_opcja,wartosc,cena_przy,wart_vat,ilosc_f,proc_vat,dieta,info,;
       skladnik,konto_kosz,stano_kosz,cena,przelewem,czekiem,nr_czeku,cena_zak,;
       termin_p,nr_spec,transport,nr_kpr,longname,sub_dok,zaplacono,data_zap,;
       ksef,nr_ksef,nazwisko,data_vat
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
#define D_LPSTR(x) str(asc(x)-48,3)
#define D_LPPUT(x) chr(x+48)
#endif
#ifndef A_IZ
  #define ilosc_f ilosc
#endif
#ifdef A_WE
  #define wtoT DM->warT_ewiD
#else
  memvar wtot
#endif
#ifdef A_DF
#command REPLACE [DM->]WARTOSC WITH <x> => field2bin('d_wartosc',DM->wartosc:=<x>,1)
#define wartosC (bin2d(binfieldget('d_wartosc')))
#endif

****************************************************************************
PROCEDURE dminput
MEMVAR lastlevel,changed
local m:=lastlevel
#ifdef A_SUBDOK
local _menudm:="    "
#else
local _menudm:="  "
#endif
   private changed:=.f.
   DO WHILE .T.
      setpos(6,maxcol()/2-10)
      SETKEY(4,{||KIBORD(CHR(27)+CHR(24)+CHR(13))})
      SETKEY(19,{||KIBORD(CHR(27)+CHR(5))})
      if !aczojs(dokumenty[MAG_POZ],@_menudm,@m)
         SET KEY 4 TO
         SET KEY 19 TO
         EXIT
      ENDIF
      SET KEY 4 TO
      SET KEY 19 TO
      lastlevel:=m
      dok_in(mag_biez,_MENUDM)

   ENDDO

RETURN
*********************
PROCEDURE dok_in
local a,x,y
#ifdef A_DOKCOMP
#ifdef __HARBOUR__
#define EVLINE self:=evline(buf,j++,@x);IF self<>NIL;IF self[3]<>NIL;__mvPrivate(Self[3]);END;x:=&(self[1]);END
#else
#define EVLINE self:=evline(buf,j++,@x);IF self<>NIL;IF self[3]<>NIL;x:=Self[3];PRIVATE &x;END;x:=&(self[1]);END
#endif
static apcomp:={}
#else
local _f,_s
#endif
parameters mag_biez,dok,nk,przegl
memvar dok,nk,operator,mag_biez,lam,NOWYDM,przegl
#ifdef A_DOKCOMP
memvar _f,_s,j,defa,linecount,buf,self,getlist
private _f,_s,linecount,buf,self
#endif
PRIVATE lam,NOWYDM,posproc:=0,canopen,nkp,mknk
private da,dd,dv,d_o,n_f,nim,nz,il,kh,gil
#ifdef A_WA
private chg_cen:=.f.,ce,ck,wa
#else
private dok_w1
#endif
#ifdef A_KASA
 #define A_ODDO
#endif
#if defined(A_ODDO) .OR. defined(A_DIETA) .OR. defined(A_KSEF)
 #define D_DIETA_OR_ODDO
#endif
#ifdef A_F9
  private parr,ppos,ppush:=.f.,pflag
#endif
#ifdef D_DIETA_OR_ODDO
  private dflag,darr,dpos,dpush:=.f.
#endif
#ifdef A_OLZA
   private kk,sk
#endif
#ifndef wtoT
   private wtot:=0
#endif
#ifdef A_JMTOT
private itot:={}
#endif
#ifdef A_GRAM
private gtot:=0
#endif
#ifdef A_FA
   private zap,zac,nrc,uw,cz,pv,tp,wz,zaplac,dazapl,avat
#ifdef A_FAT
   private sp,st
#ifdef A_NAZWISKO
   private nazwis
#endif
#endif

#else
#ifdef A_VAT
   private vat
#endif
#endif
#ifdef A_KPR
private npr
#endif
#ifdef A_KSEF
private n_ksef,xml_ksef
#endif
#ifdef A_MM
  if empty(mag_biez)
     if ascan(dokumenty[max(1,mag_poz)],dok)=0
       mag_biez:=magazyny[1]
     else
       mag_biez:=left(magazyny[max(1,mag_poz)],2)
     endif
  endif
#endif
  private mag_poz:=max(1,ascan(magazyny,mag_biez))
  private r:=MAX(1,ascan(dokumenty[MAG_POZ],dok))
  private pm:=if(dok_p_r="P",1,-1)
  if type('changed')='U'
     PRIVATE changed
  endif
#ifndef STANY
  private nowystan
  SELECT STANY
  SET ORDER TO TAG STAN_MAG
  SET FILTER TO
  SET RELATION TO
#endif
#ifndef A_SUBDOK
if dok_kh
#endif
  select firMy
  set order to TAG FIRM_NUM
  set filter to
#ifdef A_AF
  SET RELATION TO
  sel('KH','KH')
  SET RELATION TO
#endif
#ifndef A_SUBDOK
endif
#endif


#ifdef A_LAN
#ifndef A_DDBF
  #define A_DDBF
#endif
#endif

#ifdef A_DDBF
   select daty
   goto 1
#else
   #define DatY MEMVAR
#endif

   SELECT INDX_MAT
      SET FILTER TO
      SET ORDER TO TAG INDX_NUM
    SET RELATION TO
   SELECT MAIN
      SET FILTER TO
      SET ORDER TO TAG MAIN_NRK
      SET RELATION TO
   SELECT DM
      SET ORDER TO 1
      SET FILTER TO
      SET RELATION TO
  if nk#NIL
#ifdef wtoT
    nkp:=subs(nk,6)
    if empty(nkp) .and. D_LPVAL(pozycja)>maxrow() .and. pozycja>=D_LP1
       nkp:=pozycja
    endif
#else
    nkp:=NIL
#endif
    nk:=left(nk,5)
    if dbseek(KEY_PAR+nk,.f.)
#ifdef A_SUBDOK
    a:=r
    dok:=smb_dow+sub_dok
    r:=ascan(dokumenty[MAG_POZ],dok)
    if r=0
       r:=a
       dok:=smb_dow
    endif
#endif
    if dok_lpm<0
       przegl:=.t.
       canopen:=.f.
    elseif przegl#.t.
       canopen:=data>max(DatY->d_z_mies1,DatY->data_gran)
#ifdef A_OLZA
       przegl:=!canopen
#else
       przegl:=!canopen .OR. kto_pisal#HB_UTF8CHR(0x00A0)
#endif
#ifdef A_LAN
       if !przegl
          begin sequence
          if przegl:=!reclock(.t.,"Blokada możliwości poprawy.;(ktoś inny poprawia ...);Omiń - oglądanie bez poprawy",,,recno())
             canopen:=.f.
          else
             daty->(dbgoto(1))
             if data<=max(DatY->d_z_mies1,DatY->data_gran)
                canopen:=.f.
                przegl:=.t.
             endif
          endif
          recover
          return
          end sequence
       endif
#endif
#ifdef A_SUBDOK
    if !przegl
    dok:=smb_dow+sub_dok
    r:=ascan(dokumenty[MAG_POZ],dok)
    if r=0
      a:={}
      dok:=trim(dok)
      aeval(dokumenty[mag_poz],{|x|if(x=dok,aadd(a,x),)})
      if len(a)=0
         dok:=smb_dow
         r:=ascan(dokumenty[MAG_POZ],dok)
      else
         dok:=pad(dok,4)
         if !aczojs(a,@dok,0,"Wybierz rodzaj dokumentu")
            return
         endif
         r:=ascan(dokumenty[MAG_POZ],dok)
         dm->sub_dok:=subs(dok,3)
      endif
    endif
    pm:=if(dok_p_r="P",1,-1)
    endif
#endif
    else
#ifdef A_LAN
       canopen:=valtype(changed)#"L"
#else
       canopen:=changed#.f.
#endif
    endif
    ELSE
      return
    ENDIF
    NOWYDM:=.F.
    //nk:=val(nk)
  ELSE
    if dok_lpm<0
       return
    endif
    PRZEGL:=.F.
    NOWYDM:=.T.
#ifdef A_OLDA
#define D_OLZA +31
#else
#define D_OLZA
#endif
    if stary_rok=max(DatY->d_z_mies1,DatY->data_gran) .or. year(max(DatY->d_z_mies1,DatY->data_gran)+1 D_OLZA)>year(DatY->d_z_rok+1 D_OLZA)
       alarm("Cały rok zamknięty",,,3)
       return
    endif
//    DEFAULT mknk TO 4
//    nk:=A_MKNK(1)
  ENDIF
  private _SRAMKA:="G+/B"
  IF PRZEGL
    select main
    set order to TAG MAIN_NRK
    SEEK DM->(KEY_DOK+NR_DOWODU)
    select dm
    _f:=asize({0,79,0,2,dok_lpm,;
         {|_f|DOK10(_f),dok11(_f)},;
         {||NIL},{||NIL},;
         {|_f|DBSELECTAREA("MAIN"),ORDSETFOCUS("MAIN_NRK"),DBSEEK(DM->(KEY_DOK+NR_DOWODU)+D_LPPUT(_fi),.f.)},;
         {|_f|dok41(_f)},;
         {|_f|dok7(_f)}},_fLEN)
  ELSE
#ifdef A_HLINK
private HLINK
#endif
#ifdef A_NVAT
    private chgpos
#endif
    _s:=array(_sLEN)
    _f:=asize({0,79,0,2,dok_lpm,;
         {|_f|DOK1(_f),dok10(_f)},;
         {|_f,g|dok2(_f,g)},;
         {|_f,g|dok3(_f,g)},;
         {|_f|DBSELECTAREA("MAIN"),ORDSETFOCUS("MAIN_NRK"),DBSEEK(DM->(KEY_DOK+NR_DOWODU)+D_LPPUT(_fi),.f.)},;
         {|_f,g|dok4(_f,g,_s)},;
         {|_f,g|dok5(_f,g)},;
         {|_f|dok6(_f)}},_fLEN)
  ENDIF
  IF _flpmax<0
#ifdef A_LPNUM
    _flpmax:=D_LPVAL(replicate('9',A_LPNUM))
#else
    _flpmax:=D_LPVAL(chr(255))
#endif
  endif
#ifdef A_DOKCOMP
    private self,j,getlist
  if valtype(dok_co)$"MC"
     if len(dok_co)<=12
        buf:=dok_co
        if !"."$buf
           buf+=".ppd"
        endif
        j:=ascan(apcomp,{|x|x[1]==buf})
        if j#0
           dok_co:=apcomp[j,2]
        else
           j:=findfile(buf)
           if !empty(j)
              dok_co:=getlines(memoread(j))
              aadd(apcomp,{buf,dok_co})
           endif
        endif
     else
        dok_co:=getlines(dok_co)
     endif
  endif
    buf:=dok_co
    linecount:=len(buf)
    j:=1
    do while j>0 .and. j<=linecount
       EVLINE
    enddo
    if _f#NIL
       FORM_EDIT(_f)
    endif
#else
    FORM_EDIT(_f)
#endif
    select dm
    unlock recno()

RETURN
**********************************************
procedure dok1(_f)
#ifdef A_LAN
  #define D_LAN .and. rlock()
#else
  #define D_LAN
#endif
#ifdef A_VAT
#ifdef A_FK
  #define D_WAR .and. wart_vat=0 .and. zaplacono=0
#else
  #define D_WAR .and. wart_vat=0
#endif
#else
  #define D_WAR
#endif
  local x,i,p,s
  @ 0,0
  @ 0,0 say trim(magazyny[mag_poz])+" "+adres_mag[mag_poz]
  IF NOWYDM
    IF KEY_DOK<>KEY_PAR
      seek KEY_PAR LAST
    endif
    while !BOF() .and. KEY_DOK=KEY_PAR .and. pozycja=D_LP0 D_WAR D_LAN //jak blokada to zajęty
      skip -1
    enddo
//-------------------------------------
      IF !BOF() .and. KEY_DOK=KEY_PAR //zerowka tez dobra na wzór
          x:=5
          for i:=5 to 1 step -1
            s:=subs(nr_dowodU,i,1)
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
          p:=recno()
            for x:=i-1 to 1 step -1
              s:=subs(nr_dowodU,x,1)
              if !isdigit(s)
                if s<>' '
                  exit
                endif
                seek KEY_PAR+left(nr_dowodU,x) + HB_UTF8CHR(0x00A0) 
                while KEY_DOK=KEY_PAR .and. pozycja=D_LP0 D_WAR D_LAN
                  skip
                ENDDO
                IF KEY_DOK<>KEY_PAR
                   exit
                ENDIF
                ++x
                loop
              endif
            next x
          goto p
          seek KEY_PAR+left(nr_dowodU,x) LAST

          while !BOF() .and. KEY_DOK=KEY_PAR .and. pozycja=D_LP0 D_WAR D_LAN
            skip -1
          enddo
          if x>0 .and. str(1+val(subs(nr_dowodU,x+1)),i-x)='*'
             --x
          endif
          nk:=left(nr_dowodU,x)+str(1+val(subs(nr_dowodU,x+1)),i-x)+subs(nr_dowodU,i+1)
          goto p
      else
        DEFAULT mknk TO 4
        nk:=A_MKNK(1)
      endif
//--------------------------------------
      da:=max(DatE(),max(DatY->d_z_mies1,DatY->data_gran)+1)
      do while YEAR(da D_OLZA)>YEAR(DatY->D_Z_rok+1 D_OLZA)
         da-=day(da)
      enddo
#ifdef A_FA
#ifdef A_FK
      zaplac:=0
      dazapl:=ctod("")
#endif
      zac:=zap:=0
      avat:={} //getVAT(); aeval(avat,{|x,i|avat[i,2]:=0})
      nrc:=space(len(nr_czeku))
      dv:=tp:=da
      uw:="BEZ UWAG "
#else
#ifdef A_VAT
      vat:=0
#endif
#endif
#ifdef A_KPR
      npr:=DatY->last_kpr+1
#endif
      dd:=da
      _flp:=0
      n_f:=nr_faktury
#ifdef A_KSEF
      xml_ksef:=''
      n_ksef:=''
#endif
#ifdef A_KHSEP
      kh:=kontrahent
#else
      kh:=left(dm->dost_odb,A_NRLTH)
#endif
      if dok_kh.and.firMy->(dbseek(kh))
#ifdef A_FFULL
         d_o:=PAD(firMy->(trim(nazwa)+" * "+longname),LEN(DOST_ODB))
#ifdef A_AF
         KH->(dbseek(kh))
#endif
#else
#ifdef A_OLZBY
         d_o:=firMy->nazwa
#else
         d_o:=PAD(firMy->nazwa,LEN(DOST_ODB))
#endif
#endif
#ifdef A_KSEF
         if dok_p_r="P"
      x:=strtran(firmy->ident,'-')
      if !isdigit(x)
         if x='PL'
           x:=subs(x,3)
         else
           x:=''
         endif
      endif
      n_ksef:=pad(if(empty(x),'',left(x,10)+'-'+left(dtos(da),6)),len(nr_ksef))
         endif
#endif
      else
         d_o:=dost_odb
         kh:=space(A_NRLTH)
      endif
      IF KEY_DOK=KEY_PAR
          seek KEY_PAR LAST
          while !BOF() .and. KEY_DOK=KEY_PAR .and. pozycja=D_LP0 D_WAR D_LAN
             skip -1
          enddo
          IF BOF() .or. KEY_DOK<>KEY_PAR
             //nk:=A_MKNK(1) po co, wyzej jest
          else
            skip
            IF KEY_DOK<>KEY_PAR
              skip -1
            ENDIF
          ENDIF
/********************************
          if nk<=0
            nk-=2
          endif
*******************************/

          if pozycja=D_LP0 D_WAR D_LAN
      nr_dowodu:=nk //  str(nk,5)
      _fpopkey:=.t.
      nowydm:=.f.
      data:=da
      data_dost:=dd
#ifdef A_KSEF
      nr_ksef:=n_ksef
      binfieldput('KSEF',xml_ksef)
#endif
      wtoT:=0
#ifdef A_SUBDOK
      sub_dok:=subs(dok,3)
#endif
#ifdef A_VAT
      wart_vat:=0
#endif
#ifdef A_FA
#ifdef A_FK
      zaplacono:=0
      data_zap:=ctod("")
#endif
      czekiem:=przelewem:=0
      replace wartosc with 0
      uwagi:=nr_czeku:=""
      termin_p:=ctod("")
#ifdef A_DATAVAT
      data_vat:=dv
#endif
      UW = "BEZ UWAG "
      putVAT()
#endif
          endif
      ENDIF
  ELSE
      DEVOUT("  POPRAWA  !","GR+*")
      if mknk=NIL
          x:=5
          for i:=5 to 1 step -1
            s:=subs(nr_dowodU,i,1)
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
          //DEFAULT mknk TO i
          mknk:=i
      endif
      _flp:=MIN(_flp,D_LPVAL(pozycja))
      da:=DATA
      dd:=DATA_DOST
      n_f:=nr_faktury
#ifdef A_KSEF
      n_ksef:=nr_ksef
      xml_ksef:=BINFIELDGET('KSEF')
#endif
#ifdef A_DATAVAT
      dv:=data_vat
#endif
#ifdef A_KHSEP
#define D_KHPREF
      kh:=kontrahent
#else
#define D_KHPREF numer_kol+" "+
      kh:=left(dm->dost_odb,A_NRLTH)
#endif
      if dok_kh.and.firMy->(dbseek(kh))
#ifdef A_FFULL
         d_o:=PAD(firMy->(trim(nazwa)+" * "+longname),LEN(DOST_ODB))
#ifdef A_AF
         KH->(dbseek(kh))
#endif
#else
#ifdef A_OLZBY
         d_o:=firMy->nazwa
#else
         d_o:=PAD(firMy->nazwa,LEN(DOST_ODB))
#endif
#endif
      else
         d_o:=dost_odb
         kh:=space(A_NRLTH)
      endif
#ifdef A_KPR
      npr:=if(_flp=0,DatY->last_kpr+1 ,val(nr_kpr))
#endif
#ifdef A_FA
#ifdef A_FK
      zaplac:=zaplacono
      dazapl:=data_zap
#endif
      getVAT()
      zac:=czekiem
      zap:=przelewem
      nrc:=nr_czeku
      tp:=termin_p
      IF ""=(UW:=UWAGI)
         UW = "BEZ UWAG"
      ENDIF
      uw+=" "
#else
#ifdef A_VAT
      vat:=wart_vat
#endif
#endif
    if _flp=0
      _fpopkey:=.t.
    else
      KIBORD(CHR(3))
    endif
  ENDIF
//#ifndef A_OLZBY
//#endif
#ifdef wtoT
#ifndef A_GRAM
#ifndef A_JMTOT
   posproc:=_flp
#endif
#endif
#endif
#ifdef A_OLZA
  kk:=konto_kosz
  sk:=stano_kosz
#endif
#ifdef A_FAT
  sp:=nr_spec
  st:=transport
#ifdef A_NAZWISKO
  nazwis:=nazwisko
#endif
#endif
#ifdef D_DIETA_OR_ODDO
  dflag:=.f.
  darr:={}
  dpos:=1
#endif
#ifdef A_F9
  pflag:=.f.
  ppos:=0
#endif
#ifdef A_FA
#ifdef A_NVAT
  chgpos:=!nowydm .and. subs(dok,2)#"K"
#endif
/**********
  if nowydm .and. dok_p_r="F"
     go lastrec()+1
  endif
********/
#endif
return
**********************************************
PROCEDURE DOK10(_f)

  _flp:=MIN(_flp,D_LPVAL(DM->pozycja))
  IF !empty(nkp)
    _fi:=max(1,D_LPVAL(nkp))
    _fl:=min(_flp,_fi+5)
    nkp:=NIL
  endif

  set color to (_sbkgr)
  @ 1,0,6,79 BOX UNICODE '╔═╗║║ ║║ '
  if dok_kh
    @ 1,2 SAY if(pm=-1,"Odbiorca:","Dostawca:")
    @ 1,12 SAY trim(dok_naz)
  else
    @ 1,2 SAY trim(dok_naz)
    @ 2,2 SAY if(dok="K",hb_UTF8ToStr("   Powód:"),if(pm=-1,"Odbiorca:","Dostawca:"))
  endif
_frow:=2

#ifdef A_FA
  if dok_p_r="F"
    @ 5,0,11,79 BOX UNICODE '╟─╢║║ ║║ '
    _frow:=7
 #ifdef A_FK
   if dok_fk
   @ _frow,2 say "Zapłacono:" UNICODE
   @ _frow,26 say "dnia:"
   @ _frow,43 say "pozostało:" UNICODE
   ++_frow
   endif
 #endif
 #ifdef A_FAT
    //if right(dok,1)="K"
    //_frow:=7
    //else
    setpos(_frow,1)
  #ifdef A_NAZWISKO
    sayl "Nazwisko:"
    setpos(row(),col()+min(20,len(nazwisko))+1)
  #endif
  #ifdef A_CENSPEC
    sayl "Cennik:"
    setpos(row(),col()+min(18,len(nr_spec))+1)
  #else
    sayl "Nr spec.:"
    setpos(row(),col()+len(nr_spec)+1)
  #endif
    sayl "Transport:"
    ++_frow
 #endif
#ifdef A_KSEF
    setpos(_frow,2)
     ?? 'Nr KSeF:                                     eF:'
    ++_frow
#endif
 #ifdef A_ODDO
  #ifdef A_GOCZ
   #define D_ODDO "Nr zamówienia Data wysyłki"
  #else
   #define D_ODDO if(dok$DOK_ZB,"Za okres od        do     ","Nr zamówienia Data wysyłki")
  #endif
 #else
  #define D_ODDO "Nr zamówienia Data wysyłki"
 #endif
 #ifdef A_KPR
  #define D_KPR
 #else
  #define D_KPR +"           Data    Nr Kol"
 #endif
    if dok_zew$"UV"
 #ifdef A_CENVAT
       @ 3,2 say D_ODDO+" "+WANAZ+" w tym          " D_KPR UNICODE
  #define WBR(w,v) w
 #else
       @ 3,2 say D_ODDO+" "+WANAZ+"                " D_KPR UNICODE
  #define WBR(w,v) (w+v)
 #endif
 #define ZAG(w,v) ROUND(WBR(w,v)-zap-zac,A_ZAOKR)
       if subs(dok,2)="K"
          @ 3,2 say "Dokument poprawiany z dnia:"
       endif
       @ 3,50 say "VAT"
    else
       @ 3,2 say D_ODDO+"    "+WANAZ+"             "  D_KPR
    endif
 #ifdef A_KPR
    if ""=dok_kpr
       devout("           Data    Nr Kol")
    else
       devout("   Data   Nr KPR   Nr Kol")
    endif
 #endif
 #undef D_ODDO
 #undef D_KPR
    @ 5,7 say "gotówką───────przelewem─────────kartą───────Nr karty───Termin płatności" UNICODE
    @ _frow+1,0,_frow+4,79 BOX UNICODE if(pozycja>D_LP1.and.!nowydm,'╠═╣║╜─╙║ ','╠═╣║╝═╚║ ')
    @ _frow+1,2 say 'Lp══════Materiał═══════════════════════Ilość════════Cena zbytu══'+WANAZ UNICODE
    return
    *******
  endif
 #ifdef A_FK
   if dok_fk
   ++_frow
   @ _frow,2 say "Termin:"
   @ _frow,20 say "Zapłacono:" UNICODE
   @ _frow,43 say "dnia:"
   @ _frow,59 say "pozost:"
   endif
 #endif
#endif A_FA
  if dok_zew#"W" .or. dok="K"
     if dok_p_r='P'
#ifdef A_KSEF
    ++_frow
    setpos(_frow,2)
     ?? 'Nr KSeF:                                     eF:'
#endif
     endif
    _frow+=2
    setpos(_frow-1,2)
    do case
    case dok="K";?? "Dok. poprawiany   z dnia   "
    case pm=-1      ;?? hb_UTF8ToStr("Dok wysyłki   z dnia   ")
#ifdef A_ODDO
    case dok$DOK_ZB
          ?? "Za okres od        do  "
#endif
    otherwise
#ifdef A_KSEF_0
          ?? "Nr  faktury            "
#else
          ?? "Nr  faktury   z dnia   "
#endif
    endcase
#ifdef A_VAT
    if dok_zew$"UV"
#ifdef A_PZBRUT
       ?? hb_UTF8ToStr("     WARTOŚĆ   w tym VAT")
#else
       ?? hb_UTF8ToStr("     WARTOŚĆ         VAT")
#endif
    endif
#endif
endif
#ifdef A_OLZA
    if dok_zew="W" .and. ""#dok_kon
       @ _frow-1,49 say "Konto"
       @ _frow-1,55 say "Stanowisko"
    endif
#endif

    setpos(_frow-1,54)
#ifdef A_KPR
    if ""=dok_kpr
#endif

#ifdef A_DATAVAT
    if dok_zew$'UV' .and. pm=1
       devout("Data VAT  Data    Nr Kol")
    else
#endif
       devout("          Data    Nr Kol")
#ifdef A_DATAVAT
    endif
#endif
#ifdef A_KPR
    else
       devout("    Data   Nr KPR Nr Kol")
    endif
#endif
  @ _frow+1,0,_frow+4,79 BOX UNICODE if(pozycja>D_LP1.and.!nowydm,'╠═╣║╜─╙║ ','╠═╣║╝═╚║ ')
  @ _frow+1,2 say   'Lp══Kod materiału══════════════════════Ilość════════Cena════════'+WANAZ UNICODE
#ifdef A_SB
  @ _frow+1,10 SAY if(dok_p_r="S",'Stan bież.',if(""=dok_kon,'','══'+if(dok_zew="W","Zlecenie","Rodzaj"))) UNICODE
  if dok$dok_di
  @ _frow+1,36 SAY 'Stan bież.' UNICODE
  @ _frow+1,26 SAY 'Ilość' UNICODE
  endif
#else
  @ _frow+1,25 SAY if(dok_p_r="S",'Stan bież.',if(""=dok_kon,'',if(dok_zew="W","Zlecenie","Rodzaj"))) UNICODE
#endif
return
********************
procedure dok11(_f)
  set color to (_snorm)
  @ 0,0
  @ 0,0 say trim(magazyny[mag_poz])
    if canopen
       devout(hb_UTF8ToStr(" ZAMKNIĘTY"),"W+")
#ifdef A_KONTROLA
       IF A_KONTROLA
#else
       IF iS_spec
#endif
          devout(" otwarcie przez [Ctrl]+[Enter]","W+")
       endif
    endif
    set color to (_sunsel)
#ifdef A_KHSEP
      kh:=kontrahent
#else
      kh:=left(dm->dost_odb,A_NRLTH)
#endif
      if dok_kh.and.firMy->(dbseek(kh))
#ifdef A_FFULL
         d_o:=firMy->(trim(nazwa)+" * "+longname)
#ifdef A_AF
         KH->(dbseek(kh))
#endif
#else
         d_o:=firMy->nazwa
#endif
      else
         d_o:=dost_odb
         kh:=space(A_NRLTH)
      endif
#undef D_KHPREF
    if dok_kh
       @ 2,11-A_NRLTH SAY kh
    endif
#ifdef A_OLZA
    @ 2,12 say left(d_o,if(_frow<4,if(dok_zew="W" .and. ""#dok_kon,35,40),66))
#else
    @ 2,12 say left(d_o,if(_frow<4,40,66))
#endif
#ifdef wtoT
#ifndef A_GRAM
#ifndef A_JMTOT
  posproc:=D_LPVAL(pozycja)
#endif
#endif
#endif
  _flp:=D_LPVAL(pozycja)
#ifdef A_FA
  if dok_p_r="F"
    @ 4,2 SAY nr_faktury picture "@S13"
    @ 4,16 SAY data_dost
#ifdef A_KPR
    if ""=dok_kpr
#endif
    @  4,61 say da:=data
#ifdef A_KPR
    else
    @  4,55 say da:=data
    @  4,66 say nr_kpr
    endif
#endif
    getVAT()
    @ 4,73 say nk PICTURE "XXXXX"
    @ 6,19 say zap:=przelewem picture WAPIC
    @ 6,35 say zac:=czekiem picture WAPIC
    @ 6,4  say ZAG(wartosc,wart_vat) picture WAPIC
    @ 6,51 say nr_czeku
    @ 6,65 say termin_p
    setpos(7,1)
#ifdef A_FK
    if dok_fk
    @ row(),13 say zaplac:=zaplacono picture WAPIC
    @ row(),32 say data_zap
    setpos(8,1)
    endif
#endif
#ifdef A_FAT
#ifdef A_NAZWISKO
    @ row(),11+col() say nazwisko picture "@S20K"
#endif
    @ row(),9+col() say nr_spec picture "@S18K"
    @ row(),12+col() say transport picture "@KS"+ltrim(str(_fco2-col()-3))
    setpos(row()+1,1)
#endif
#ifdef A_KSEF
    @ _frow-1,11 SAY nr_ksef
    @ _frow-1,50 SAY binfieldget('KSEF') picture "@S28"
#endif
    @ _frow,2 say pad(uwagi,76)
    return
#ifdef A_DATAVAT
  elseif dok_zew$"UV" .and. pm=1
    @ _frow,50 say data_vat
#endif
  endif
#endif
#ifdef A_OLZA
    if dok_zew="W" .and. ""#dok_kon
       @ 2,48 say konto_kosz
       @ 2,54 say stano_kosz
    endif
#endif
  if dok_zew#"W" .or. dok="K"
    @ _frow,2 SAY nr_faktury picture "@S13"
    sayl data_dost
  ENDIF
#ifdef A_FK
    if dok_fk
    @ 3,9 say termin_p
    @ 3,30 say zaplac:=zaplacono picture WAPIC
    @ 3,48 say data_zap
    endif
#endif
#ifdef A_KPR
    if ""=dok_kpr
#endif
    @  _frow,61 say da:=data
#ifdef A_KPR
    else
    @  _frow,55 say da:=data
    @  _frow,66 say nr_kpr
    endif
#endif
#ifdef A_KSEF
    @ _frow-2,11 SAY nr_ksef
    @ _frow-2,50 SAY binfieldget('KSEF') picture "@S28"
#endif
    @  _frow,73 say nk  PICTURE "XXXXX"

return
**********************************************
procedure dok2(_f,getlist)
    local get,txt,x,d
    static iv
    __setproc(procname(0))
    select dm
    if dok_kh
       @ 2,11-A_NRLTH GET kh WHEN .f.
    endif
#ifdef A_OLZA
    get:=getnew(2,12,{|x|if(pcount()=0,d_o,d_o:=x)},"d_o",if(_frow=2,if(dok_zew="W" .and. ""#dok_kon,"@KS35","@KS40"),"@KS66"))
#else
    get:=getnew(2,12,{|x|if(pcount()=0,d_o,d_o:=x)},"d_o",if(_frow=2,"@KS40","@KS66"))
#endif
    get:display()
    if dok_kh
       get:postblock:={|g|szukam({1,14,maxrow(),,1,0,'FIRMY',,{|_skey,_s|gfirma(_skey,_s,getlist)},TRIM(d_o)})}
    elseif dok_zew="W" .and. dok="M"
       get:postblock:={||!empty(subs(d_o,3)).or.aczojs(magazyny),.t.}
#ifdef A_MULTIDI
    elseif dok$dok_di .and. mag_biez=A_MAGDI
       d:=aclone(stanowis[mag_poz])
       aeval(d,{|x,i|d[i]:=pad(strtran(x,"*"," "),len(d_o))})
       get:postblock:={|g|if(g:changed,(dflag:=.f.,darr:={},dpos:=1),),aczojs(d)}
#endif
    endif
    aadd(getlist,get)
#ifdef A_FA
  if dok_p_r="F"
#ifdef A_CENSPEC
      get:postblock:={|g|szukam({1,14,maxrow(),,1,0,'FIRMY',,{|_skey,_s|gfirma(_skey,_s,getlist)},TRIM(d_o)})}
#endif
#ifdef A_MM
#define D_MM
#else
#define D_MM mag_biez+
#endif

    if subs(dok,2)="K"
      @ 4,2 GET n_f PICTURE "@K!S13" valid {|x|x:=recno(),empty(n_f).and.szukam({1,1,maxrow(),,1,0,'DOKUMENTY',{||smb_dow+nr_dowodu+I+dtoc(data)+I+dost_odb},{|_skey,_s|(_sret:=_skey=13).or._skey=27.or._skey=0.and.(if(val(d_o)=0,,_sfor:={||dost_odb=left(d_o,A_NRLTH)}),dbseek(D_MM '',,.t.),dbskip(),.f.)},D_MM ''}).and.(dd:=data,n_f:=pad(KEY_DOK+nr_dowodu,len(n_f)),updated(.t.),.t.),dbgoto(x),.t.}
      @ 4,16 GET dd
//#ifndef A_NVAT
      iv:=ascan(avat,{|y|y[2]#0})
      get:=getnew(3,39,{|x|if(pcount()=0,if(iv=0,0,val(avat[iv,1])),(txt:=str(x,2),iv:=ascan(avat,{|y|y[1]=txt})))},"iv","@Z ##")
      get:display()
      aadd(getlist,get)
      @ 4,38 SAY '%' COLOR (_sunsel)
      get:=getnew(4,39,{|x|if(pcount()=0,if(iv=0,vat(),avat[iv,2]/100),avat[iv,2]:=round(x*100,0))},"vat","@KE #######.##")
      get:preblock:={||iv#0}
      get:postblock:={|g|!g:changed.or.showfakh(_f,dm->wartosc,vat())}
      get:display()
      aadd(getlist,get)
//#endif
    ELSE
#ifndef A_GOCZ
#ifdef A_ODDO
      if dok$DOK_ZB
         get:=getnew(4,4,{|x|if(x=NIL,ctod(n_f),n_f:=dtoc(x))},"n_f")
         get:display()
         aadd(getlist,get)
      else
#endif
#endif
         @ 4,2 GET n_f PICTURE "@KS13"
#ifndef A_GOCZ
#ifdef A_ODDO
      endif
#endif
#endif
      //@ 4,16 GET dd VALID {|g|if(dataval(dd).and.g:original=da.and.da#dd ,(da:=dd,x:=g,aeval(getlist,{|g|if(g:name=='da',(g:display(),eval(g:postblock,x)),)})),),.t.}
      @ 4,16 GET dd VALID {|g|if(dataval(dd),(if(g:original=da.and.da#dd ,(da:=dd,x:=g,aeval(getlist,{|g|if(g:name=='da',(g:display(),eval(g:postblock,x)),)})  ),)),),.t.}
    endif
#ifdef A_NVAT
      if chgpos .and. subs(dok,2)#"K"
         SELECT MAIN
         SET ORDER TO TAG MAIN_NRK
         SEEK DM->(KEY_DOK+NR_DOWODU)
         txt:=0
         aeval(avat,{|x|x[3]:=0})
#ifdef A_DF
         exec {|w|vat(proc_vat,0,WDFGR(pm*ilosc_f,cena,val(proc_vat),dok_df))} while KEY_DOK+nr_dowodu=dm->(KEY_DOK+nr_dowodu)
         if dok_df //brutto
 #ifdef A_CENVAT
            aeval(avat,{|v|v[2]:=v[3]*val(v[1])/(100+val(v[1])),txt+=v[3],v[3]-=round(v[2],0)})
 #else
            aeval(avat,{|v|v[2]:=v[3]*val(v[1])/(100+val(v[1])),txt+=v[3]-v[2],v[3]-=round(v[2],0)})
 #endif
         else //netto
 #ifdef A_CENVAT
            aeval(avat,{|v|v[2]:=v[3]*val(v[1])/100,txt+=v[3]+v[2]})
 #else
            aeval(avat,{|v|v[2]:=v[3]*val(v[1])/100,txt+=v[3]})
 #endif
         endif
#else
         exec {|w|vat(proc_vat,0,WGR(pm*ilosc_f,cena,val(proc_vat),dok_df))} while KEY_DOK+nr_dowodu=dm->(KEY_DOK+nr_dowodu)
 #ifdef A_CENVAT
         aeval(avat,{|v|v[2]:=v[3]*val(v[1])/(100+val(v[1])),txt+=v[3]})
 #else
         aeval(avat,{|v|v[2]:=v[3]*val(v[1])/100,txt+=v[3]})
 #endif
#endif
//          endif
          select DM
#ifdef A_A
          x:=dbstruct()
          aeval(x,{|x,y,z|if(x[2]$'BN'.and.x[1]='WART_NET',(vat(lower(subs(x[1],9)),,@z),fieldput(y,Round(z,0)/100)),)})
          aeval(x,{|x,y,z|if(x[2]$'BN'.and.x[1]='WART_VAT',(vat(lower(subs(x[1],9)),@z,),fieldput(y,Round(z,0)/100)),)})
          //aeval(avat,{|x|dm->(fieldput(fieldpos('wart_net'+ltrim(x[1])),x[3]/100)),if(val(x[1])#0,dm->(fieldput(fieldpos('wart_vat'+ltrim(x[1])),round(x[2],0)/100)),)})
#endif
          REPLACE wartosc WITH txt/100
          putVAT()
          txt:=NIL
          chgpos:=.f.
          @ 4,26 say dm->wartosc PICTURE WAPIC COLOR _Sbkgr
          @ 4,39 say dm->wart_vat PICTURE "@E #######.##" COLOR _Sbkgr
          @ 6,4 say ZAG(dm->wartosc,dm->wart_vat) PICTURE WAPIC COLOR _Sbkgr
#ifdef A_FK
          if dok_fk
             @ _frow-1,54 say WBR(dm->wartosc,dm->wart_vat)-zaplac PICTURE WAPIC COLOR _Sbkgr
          endif
#endif
      endif
#endif
#ifdef A_KPR
    if ""=dok_kpr
#endif
    @ 4,61 GET da VALID {|g,x|if((x:=dataval(da)).and.g:changed.and.tp=g:original+if(zap=0,0,dok_tpd),(tp:=da+if(zap=0,0,dok_tpd),getlist[ascan(getlist,{|x|x:name="tp"})]:display(),.t.),x)}
#ifdef A_KPR
    else
    @ 4,55 GET da VALID {|g,x|if((x:=dataval(da)).and.g:changed.and.tp=g:original+if(zap=0,0,dok_tpd),(tp:=da+if(zap=0,0,dok_tpd),getlist[ascan(getlist,{|x|x:name="tp"})]:display(),.t.),x)}
    @ 4,66 get npr picture "@K #####" valid {|g|!g:changed .or. if(nowydm,npr=DatY->last_kpr+1,npr=val(nr_kpr)) .or. NIL#alarm("Czy numer kolejny książki nie powinien być:"+if(nowydm,str(DatY->last_kpr+1,5),nr_kpr))}
    endif
#endif
    @ 4,73 GET nk PICTURE "@!K XXXXX" VALID {|g|!g:changed.or.nk(_f,g)}
    get:=getnew(6,19,{|x|if(pcount()=0,zap,zap:=ROUND(x,A_ZAOKR))},"zap",WAPIC)
    get:postblock:={|g|setpos(6,4),devout(strpic(ZAG(dm->wartosc,dm->wart_vat),12,A_ZAOKR,"@E "),_sbkgr),;
    if(if(g:original=0,zap#0,zap=0).and.tp=da+if(zap#0,0,dok_tpd),(tp:=da+if(zap=0,0,dok_tpd),getlist[ascan(getlist,{|x|x:name="tp"})]:display()),),.t.}
    get:reader:={|g|setkey(61,{|p,g|g:=getactive(),p:=setkey(61,NIL),zap:=WBR(dm->wartosc,dm->wart_vat)-zac,g:changed:=.t.,setkey(61,p)}),getreader(g),setkey(61,NIL)}
    get:display()
    aadd(getlist,get)
    get:=getnew(6,35,{|x|if(pcount()=0,zac,zac:=ROUND(x,A_ZAOKR))},"zac",WAPIC)
    get:postblock:={||setpos(6,4),devout(strpic(ZAG(dm->wartosc,dm->wart_vat),12,A_ZAOKR,"@E "),_sbkgr),.t.}
    get:reader:={|g|setkey(61,{|p,g|g:=getactive(),p:=setkey(61,NIL),zac:=WBR(dm->wartosc,dm->wart_vat)-zap,g:changed:=.t.,setkey(61,p)}),getreader(g),setkey(61,NIL)}
    get:display()
    aadd(getlist,get)
    @ 6,51 get nrc PICTURE "@KS13"
    @ 6,65 get tp  valid tp>=da .OR. (TP:=DA,.t.)
    setpos(7,1)
#ifdef A_FK
   if dok_fk
   @ row(),13 get zaplac picture WAPIC valid {|g|setpos(g:row(),54),devout(tran(WBR(dm->wartoSC,dm->wart_vat)-zaplac,WAPIC),_sbkgr),if(empty(dazapl).and.g:changed,(dazapl:=tp,getlist[ascan(getlist,{|x|x:name="dazapl"})]:display()),),.t.}
   @ row(),32 get dazapl
    setpos(row()+1,1)
   endif
#endif
#ifdef A_FAT
    if _frow>7
    setpos(7,1)
#ifdef A_NAZWISKO
    @ row(),11+col() get nazwis picture "@S20K"
#endif
    @ row(),9+col() get sp picture "@S18K"
    @ row(),12+col() get st picture "@KS"+ltrim(str(_fco2-col()-3))
      atail(getlist):cargo:=.t.
    endif
#endif
#ifdef A_KSEF
    @ _frow-1,11 GET n_ksef SEND block:={||n_ksef}   //tylko odczyt
    @ _frow-1,50 GET xml_ksef PICTURE "@KS28" SEND block:={|x|if(x=NIL.or.!empty(x).or.ksef_getfa(n_ksef,,@x)=NIL.and.alarm('Błąd:'+HB_EOL()+x)<>NIL,xml_ksef,xml_ksef:=x)}
#endif
    @ _frow,2 GET uw picture "@KS76" send cargo:=.t.
    return
  else
#ifdef A_FK
   if dok_fk
   @ 3,9 get tp valid {|g,r|r:=dataval(tp).and.g:changed,if(r.and.tp<dd,(dd:=tp,x:=g,aeval(getlist,{|g|if(g:name=='dd',(g:display(),eval(g:postblock,x)),)})),),if(r.and.(empty(dazapl).or.dazapl=g:original),(dazapl:=tp,getlist[ascan(getlist,{|x|x:name="dazapl"})]:display()),),.t.}
   @ 3,30 get zaplac picture WAPIC valid {|g|setpos(3,66),devout(tran(wartosc+wart_vat-zaplac,WAPIC),_sbkgr),if(empty(dazapl).and.g:changed,(dazapl:=tp,getlist[ascan(getlist,{|x|x:name="dazapl"})]:display()),),.t.}
   @ 3,48 get dazapl
   endif
#endif
#endif
  if dok_zew#"W" .or. dok="K"
#ifdef A_ODDO
#ifndef A_GOCZ
      if dok$DOK_ZB
         get:=getnew(_frow,4,{|x|if(x=NIL,ctod(n_f),n_f:=dtoc(x))},"n_f")
         get:display()
         aadd(getlist,get)
      else
#endif
#endif
#ifdef A_KSEF
      @ _frow-2,11 get n_ksef picture "@K" VALID ksef_valid() // WHEN NOWYDM .or. pozycja=D_LP0
      @ _frow-2,50 GET xml_ksef PICTURE "@S28" SEND block:={||xml_ksef}
#endif
      @ _frow,2 GET n_f PICTURE "@KS13"
#ifndef A_GOCZ
#ifdef A_ODDO
      endif
#endif
#endif
#ifdef A_DLINK
      GETl dd VALID {|g|if(dataval(dd),(if(g:original=da.and.da#dd ,(da:=dd,x:=g,aeval(getlist,{|g|if(g:name=='da',(g:display(),eval(g:postblock,x)),)})),),if(g:original=dv.and.dv#dd ,(dv:=dd,x:=g,aeval(getlist,{|g|if(g:name=='dv',(g:display(),if(g:postblock<>NIL,eval(g:postblock,x),)),)})),)),),.t.}
#else
      GETl dd
#endif
#ifdef A_VAT
      if dok_zew$"UV"
#ifdef A_FA
      iv:=ascan(avat,{|y|y[2]#0})
      get:=getnew(_frow-1,39,{|x|if(pcount()=0,if(iv=0,0,val(avat[iv,1])),(txt:=str(x,2),iv:=ascan(avat,{|y|y[1]=txt})))},"iv","@Z ##")
      get:postblock:={|g|!g:changed.or.(aeval(getlist,{|g|if(g:name=='vat',g:display(),)}),.t.)}
      get:display()
      aadd(getlist,get)
      dispout('%',_sunsel)
      get:=getnew(_frow,39,{|x|if(pcount()=0,if(iv=0,vat(),avat[iv,2]/100),avat[iv,2]:=ROUND(x*100,0))},"vat","@KE #######.##")
      get:preblock:={||iv#0}
      get:postblock:={|g|!g:changed.or.showvath(_f,dm->wartosc,vat())}
      get:display()
      aadd(getlist,get)
#ifdef A_DATAVAT
    //if dok_zew$'UV' .and. pm=1
       @ 4,50 GET dv
    //endif
#endif
#else
         get:=getnew(_frow,45,{|x|if(pcount()=0,vat,vat:=ROUND(x,A_ZAOKR))},"vat",WAPIC)
         get:display()
         aadd(getlist,get)
#endif
      endif
#endif
    ELSE
      DD:=CTOD(N_F:="")
    ENDIF
#ifdef A_FA
endif
#endif
#ifdef A_OLZA
    if dok_zew="W" .and. ""#dok_kon
       @  _frow,48 get kk PICTURE "@!K" valid gkon() .or. aczojs(kont)
       @  _frow,54 get sk PICTURE "@!K" valid {|A,B,POZ|gsta(getlist) .or. aczojs(stano,,@poz).and.(d_o#" ".or. (d_o:=padr(substr(stano[poz],7),len(d_o)),aeval(getlist,{|g|if(g:name=='d_o',g:display(),)}),.t.))}
    else
       kk:=sk:=""
    endif
#endif
#ifdef A_KPR
    if ""=dok_kpr
#endif
    @ _frow,61 GET da VALID dataval(da)
#ifdef A_KPR
    else
    @ _frow,55 GET da VALID dataval(da)
    @ _frow,66 get npr picture "@K #####"
    // valid {|g|!g:changed .or. if(nowydm,npr=DatY->last_kpr+1,npr=val(nr_kpr)) .or. NIL#alarm("Czy numer kolejny książki nie powinien być:"+if(nowydm,str(DatY->last_kpr+1,5),nr_kpr))}
    endif
#endif
    @  _frow,73 GET nk  PICTURE "@!K XXXXX" VALID {|g|!g:changed.or.nk(_f,g)}
return
**********************************************
procedure dok3(_f,getlist)
local i,mes,rarr,nk2,j,k,a,b,c,d,x,y,z
#ifdef D_DIETA_OR_ODDO
local txt
field posilek,licznik
memvar exp_od,exp_do
#endif
 if NOWYDM .or. updated() .or. changed=.t.
    i:=if(NOWYDM,0,recno())
    nk2:=KEY_DOK+NR_DOWODU
    if nk2 <> KEY_PAR+nk //STR(nk,5)
      SET ORDER TO TAG dm_trim    //nr_mag+smb_dow+padl(strtran(nr_dowodu,' '),5)
      j:=ordsetfocus()<>'DM_TRIM' .or. dbSEEK(dseek(,'nr_mag,smb_dow,nr_dowodu',mag_biez,left(dok,2),nk)) .and. recno()<>i
      //dbseek(KEY_PAR+padl(strtran(nk,' '),5)) .and. recno()<>i
      set order to 1
      if j
         if ! (pozycja=D_LP0 D_WAR D_LAN)
#undef D_LAN
#undef D_WAR
           alarm("Znalazłem inny dokument o tym samym numerze z dnia "+dtoc(data)+"!;Zmień na pierwszy wolny.",,,3)
           goto i
           LOCK
           _fposg:=findget(getlist,'nk')
           _fkey:=K_PGUP
*****************
           RETURN
*****************
         else

           NOWYDM:=.f.
         endif
      else
        goto i
      endif
     if NOWYDM
      append blank
      smb_dow := dok
#ifndef A_MM
      nr_mag := mag_biez
#endif
      kto_pisal := HB_UTF8CHR(0x00A0)+chr(0)
      data := da
#ifdef D_DIETA_OR_ODDO
      dflag:=.f.
      darr:={}
      dpos:=1
#endif

      pozycja:=D_LP0
     else
      LOCK
     endif

      if pozycja<=D_LP0
         wtoT:=0
#ifdef A_JMTOT
         itot:={}
#endif
#ifdef A_GRAM
         gtot:=0
#endif
         _flp:=posproc:=0
      endif

      POZYCJA := D_LPPUT(_flp)
      nr_dowodu := nk //STR(nk,5)
#ifdef A_SUBDOK
      sub_dok:=subs(dok,3)
#endif
      NOWYDM := .F.
      goto recno()
      if _flp<>0
          SELECT MAIN
          SET ORDER TO TAG MAIN_NRK
          SEEK nk2
          rarr:={}
          exec aadd(rarr,recno()) while KEY_DOK+nr_dowodu=nk2 rest
#ifndef A_LAN
          aeval(rarr,{|i|dbgoto(i),nr_dowodu:=nk})
#else
          // break lub .t.
          aeval(rarr,{|i|dbgoto(i),if(reclock(.F.,"NIE POTRAFIĘ ZMIENIĆ NUMERU W POZYCJI"+D_LPSTR(POZYCJA)),nr_dowodu:=nk,)})
          unlock
#endif
          select DM
      endif
    ELSE
      LOCK
    ENDIF
    IF DATA#DA
#ifdef D_DIETA_OR_ODDO
        dflag:=.f.
        darr:={}
        dpos:=1
#endif
        IF POZYCJA>D_LP0
           IF pm*(DA-DATA)>0
              alarm("ZMIENIASZ DATĘ, SPRAWDŹ,;CZY NIE POJAWIAJĄ SIĘ;STANY UJEMNE W KARTOTEKACH;NA DZIEŃ "+DTOC(IF(DATA>DA,DA,DATA)),,,3)
           ENDIF
           SELECT MAIN
           SET ORDER TO TAG MAIN_NRK
           SEEK DM->(KEY_DOK+NR_DOWODU)
#ifndef A_LAN
           replace data with da while KEY_DOK+nr_dowodu=dm->(KEY_DOK+nr_dowodu) rest
#else
           replace data with da for reclock(.F.,"NIE POTRAFIĘ ZMIENIĆ DATY POZYCJI"+D_LPSTR(POZYCJA) ,.F.) while KEY_DOK+nr_dowodu=dm->(KEY_DOK+nr_dowodu) rest
           unlock
#endif
        ENDIF
        select dm
        data := da
    ENDIF
    data_dost := dd
#ifdef A_DATAVAT
    data_vat:=if(pm=1,dv,da)
#endif
#ifdef A_KHSEP
#ifndef A_NOMZ
    c:=kontrahent
#endif
    kontrahent:=kh
    dost_odb := d_o
#else
#ifndef A_NOMZ
    c:=left(dost_odb,A_NRLTH)
#endif
    dost_odb := if(dok_kh.and.!empty(kh),kh+' '+d_o,d_o)
#endif
    nr_faktury := n_f
#ifdef A_KSEF
    nr_ksef := n_ksef
    BINFIELDPUT('KSEF',xml_ksef)
#endif
#ifdef A_OLZA
    konto_kosz:=kk
    stano_kosz:=sk
#endif
#ifdef A_FA
#ifdef A_FAT
#ifdef A_CENSPEC
    if empty(sp)  //indx_mat->(type(sp))#"N"
       sp:=pad("CENA",len(nr_spec))
       aeval(getlist,{|g|if(g:name="sp",g:display(),)})
    endif
#endif
    nr_spec:=sp
    transport:=trim(st) //bo memo
#ifdef A_NAZWISKO
    nazwisko:=nazwis
    IF kh=firmy->numer_kol .and. empty(firmy->nazwisko)
       lock in firmy
       firmy->nazwisko:=nazwis
       unlock in firmy
    ENDIF
#endif
#endif
    przelewem:=zap
    czekiem:=zac
    nr_czeku:=nrc
    putVAT()
#ifdef A_FK
    zaplacono:=zaplac
    data_zap :=dazapl
#endif
    termin_p:=tp
    if uw="BEZ UWAG"
       UWAGI:=""
    ELSE
       uwagi:=trim(uw)
    endif
#else
#ifdef A_VAT
    if dok_zew$"UV"
       WART_VAT:=vat
    endif
#endif
#endif
#ifdef A_FDO
     if kh=firMy->numer_kol .and. !firMy->(eof())
        if firMy->P_R=" "
           lock in firmy
           firMy->P_R:=if(pm=1,"P","R")
           unlock in firmy
        elseif firMy->p_r="X"
        elseif if(pm=1,"P","R")#firMy->P_R
           lock in firmy
           firMy->p_r:="X"
           unlock in firmy
        endif
     endif
#endif
#ifdef A_KPR
    if ""#dok_kpr
       nr_kpr:=str(npr,5)
    endif
#endif
#ifndef A_NOMZ
    if pozycja>D_LP0 .and. (dok_kon="&:" .or. dok_kh .and. c#kh)
           SELECT INDX_MAT
           SET ORDER TO TAG INDX_NUM
           SELECT MAIN
           SET ORDER TO TAG MAIN_NRK
           SET RELATION TO NR_MAG+INDEX INTO STANY
#ifndef STANY
           SELECT STANY
           SET ORDER TO TAG STAN_MAG
           SELECT MAIN
           SET RELATION TO INDEX INTO INDX_MAT ADDITIVE
#endif
           SEEK DM->(KEY_DOK+NR_DOWODU)
           if dok_kon="&:"
           x:=&("{||nz:=nr_zlec,MAIN->NR_ZLEC:="+trim(subs(dok_kon,3))+"}")
#ifndef A_LAN
           dbeval(x,,{||KEY_DOK+nr_dowodu=dm->(KEY_DOK+nr_dowodu)})
#else
           dbeval(x,{||RECLOCK(.F.,"NIE POTRAFIĘ ZMIENIĆ NUMERU KONTA POZYCJI"+D_LPSTR(POZYCJA) ,.F.)},{||KEY_DOK+nr_dowodu=dm->(KEY_DOK+nr_dowodu)})
           unlock
#endif
           else
           i:=len(EvAlDb(IndexKey(3)))-len(index)-A_NRLTH-10
#ifndef A_LAN
           replace nr_zlec with left(nr_zlec,i)+kh while KEY_DOK+nr_dowodu=dm->(KEY_DOK+nr_dowodu) for c==subs(nr_zlec,i+1,A_NRLTH)
#else
           replace nr_zlec with left(nr_zlec,i)+kh while KEY_DOK+nr_dowodu=dm->(KEY_DOK+nr_dowodu) for c==subs(nr_zlec,i+1,A_NRLTH) .and. reclock(.F.,"NIE POTRAFIĘ ZMIENIĆ NUMERU KONTA POZYCJI"+D_LPSTR(POZYCJA) ,.F.)
           unlock
#endif
           endif
           set relation to
           commit
           select dm
    endif
#endif
#ifdef A_AF
#ifdef A_KOB
    field->mag_fk:=''
#endif
#endif
    commit
    changed:= .t.
 endif
#ifdef A_FA
#ifdef A_KPR
#define D_POSG 8
#else
#define D_POSG 7
#endif
 if dok_p_r="F"
 #ifdef A_SZYM
    if val(kh)#0 .and. pozycja=D_LP0
       LOCK IN FIRMY
       if firmy->last_nr < nr_dowodu .and. (_fkey=K_PGDN .or. _fkey=K_ENTER)
          ++firmy->licznik
          firmy->last_nr:=nr_dowodu
       elseif firmy->last_nr=nr_dowodu .and. (_fkey=K_CTRL_L .or. _fkey=K_ESC)
          --firmy->licznik
          firmy->last_nr:=""
       endif
       UNLOCK IN FIRMY
    endif
 #endif
    if (_fkey=K_CTRL_L .or. _fkey=K_ENTER) .and. dok_kop<>0 .and. _flp>0 .and. _fposg>=D_POSG
       _fkey:=K_CTRL_L
       if dok_zew="V" .and. empty(if(kh=firmy->numer_kol.and.!firmy->(eof()),firmy->ident,nr_faktury))
          alarm('BRAK NIPU !',,3,3)
          _fkey:=K_PGUP
       elseif dok_kop<0 .or. TAK(hb_UTF8ToStr('CZY DRUKOWAĆ (DANE POPRAWNE)'),_frow+1,,.T.,.F.)
          wydruk_dok(abs(dok_kop))
       ENDIF
    endif
    _flastexit:=NIL
    _fposg:=max(D_POSG,_fposg)
 endif
#undef D_POSG
#endif

 NZ:=SPACE(a:=LEN(MAIN->NR_ZLEC))

#ifdef A_OLZA
 if dok_zew="W" .and. ""#dok_kon
    NZ:=subs(SK,4,1)+IF(SK="50","00",if(sk="52","11","  "))+RIGHT(SK,2)+"0000"
 ENDIF
#endif
#ifdef A_KSEF
  if dok$DOK_KSEF .AND. empty(darr) .and. !empty(xml_ksef)
    if xml_ksef="<?xml"
       a:=xml2json(xml_ksef) //'FaWiersz')
    else
       a:=hb_jsondecode(xml_ksef)
    endif
    a:=a['Faktura','Fa','FaWiersz']
    darr:=getlines(hb_jsonencode(a,.t.))
       if (dpos:=min(_flp+1,len(darr)))=1
          @ _frow+4,1 say 'Pozycje z KSeF pod [F8]' color _sbkgr
       endif
       dflag:=dm->pozycja=D_LP0 .and. dpos>0
  endif
#endif
#ifdef A_ODDO
    if dok$DOK_ZB .AND. empty(darr)
       mes:=message("Chwileczkę ")
begin sequence
#ifdef A_KASA
       begin sequence
       if type('path_zb')="U"
          private path_zb:=''
       endif
       if !file(path_zb+'exp_daty.mem')
          break
       endif
       restore from (path_zb+'exp_daty') additive
       if type("exp_od")="D" .and. type("exp_do")="D" .and. exp_od=CTOD(n_f) .and. exp_do=dd
          select 0
          nuse (path_zb+'exp_main') readonly exclusive
          darr:=array(lastrec())
          exec {||message(10),darr[recno()]:=pad(index,len(main->index))+str(ilosc,10,ILDEC)+" "+proc_vat+"% "+str(cena,10,A_ZAOKR)+" zł "+str(licznik,3)+" X "+nazwa+str(wartosc,10,A_ZAOKR)}
          use
       endif
       end sequence
#else
       select main
       set order to tag MAIN_ZLE
       a:=len(EvAlDb(IndexKey()))-len(index)-10
#ifdef A_GOCZ
       for k:=1 to LEN(zamowienie[mag_poz])
          if zamowienie[mag_poz,k]<>'Z'
             LOOP
          endif
          txt:=UpP(left(zamowienie[mag_poz,k],a-A_NRLTH)+kh+mag_biez)
       seek txt
       y:=.t.
       do while txt=UpP(nr_zlec)
          txt:=UpP(left(zamowienie[mag_poz,k],a-A_NRLTH)+kh+mag_biez+index)
          seek txt+dtos(ctod(sp))
          do while UpP(left(nr_zlec,a)+nr_mag+index)=txt .and. data<=ctod(st)
          c:=j:=i:=0
          z:=.t.
          exec {||c+=cena,i+=ilosc_f,j+=field->ilosc_rozl,z:=z.and.field->rozliczone} rest while {||message(10),UpP(nr_zlec+nr_mag+index)=txt .and. data<=ctod(st)}
          c:=ROUND(c/i,A_ZAOKR)
          if i#0
#ifdef STANY
             indx_mat->(dbseek(subs(txt,a+1),.f.))
#else
             indx_mat->(dbseek(subs(txt,a+3),.f.))
#endif
             lam:=i_lam(da)
             x:="   "
             if !z
               //message(mes)
               //mes:=Message('Nie rozliczone:;'+trim(indx_mat->nazwa)+';Naciśnij klawisz!')
               y:=.f.
               x:="<#>"
             endif
             aadd(darr,indx_mat->index+" "+str(j,10,ILDEC)+" "+(lam)->jm+" po "+str(c,10,A_ZAOKR)+" zł z "+str(i,11,ILDEC)+x+trim((lam)->nazwa))
          endif
          enddo
          if left(nr_zlec,a)+nr_mag+index=txt
             seek txt + HB_UTF8CHR(0x00A0)
          endif
       enddo
       next k
       if y
         message(mes)
         mes:='Nie rozliczone:'
         aeval(darr,{|x|if("<#>"$x,mes+=";"+subs(x,44),)})
         mes+=';Naciśnij klawisz...'
         mes:=message(mes)
         tone(130,3)
         inkey(0)
       endif
#else
       seek pad("D",a-A_NRLTH)+kh+mag_biez
       do while nr_zlec=pad("D",a-A_NRLTH)+kh
          txt:=pad("D",a-A_NRLTH)+kh+mag_biez+index
          seek txt+dtos(ctod(n_f))
          do while left(nr_zlec,a)+nr_mag+index=txt .and. data<=dd
          c:=j:=i:=0
          exec {||c+=wartosc,i+=ilosc,++j} rest while {||message(10),left(nr_zlec,a)+nr_mag+index=txt .and. data<=dd}
          c:=ROUND(c/i,A_ZAOKR)
          if i#0
#ifdef STANY
             indx_mat->(dbseek(subs(txt,a+1),.f.))
#else
             indx_mat->(dbseek(subs(txt,a+3),.f.))
#endif
             lam:=i_lam(da)
             aadd(darr,indx_mat->index+" "+str(pm*i,10,ILDEC)+" "+(lam)->jm+" po "+str(c,10,A_ZAOKR)+" zł "+str(j,3)+" X "+trim((lam)->nazwa))
          endif
          enddo
          if left(nr_zlec,a)+nr_mag+index=txt
             seek txt + HB_UTF8CHR(0x00A0)
          endif
       enddo
#endif
       set order to tag MAIN_NRK
#endif
       if (dpos:=min(_flp+1,len(darr)))=1
           @ _frow+4,1 say 'Zbiorówka za ten okres pod [F8]' UNICODE color _sbkgr
       endif
       dflag:=dm->pozycja=D_LP0 .and. dpos>0
recover
      dflag:=.f.
      darr:={}
      dpos:=1
end sequence
       select dm
       message(mes)
    endif
#endif //oddo
#ifdef A_DIETA
#ifdef A_MULTIDI
    if empty(darr) .and. pm=-1 .and. dok$dok_di .and. dok_zew="W" .and. nr_mag=A_MAGDI
       mes:=message(hb_UTF8ToStr("Chwileczkę "))
begin sequence
       dflag:=.f.
       darr:={}
       sel("SUROWCE","SUR_KOD",.t.)
       sel("INDEKS")
       locate for INDEKS->PLIK='ZAPOT' .and. d_o=right(trim(INDEKS->baza),1)
       IF FOUND()
          j:=recno()
          sel(TRIM(INDEKS->baza),"ZAP_REL",.t.)
          INDEKS->(dbgoto(j))
          j:=Trim(SUBS(d_o,2,1))
#ifdef A_ONEPOS
          IF EMPTY(j)
             j:=SUBS(d_o,a+2)
             j:=LEFT(j,AT(' ',j)-1)
          ENDIF
#endif
          for i=1 to len(posilki)
          c:=left(posilki[i],1)
          d:=dtos(da)+c
          if dbseek(d) .and. (empty(j) .or. c$j)
            dflag:=.t.
            c:=right(trim(indeks->baza),1)+if(empty(j),c,left(j,1))
            exec {||message(10),aadd(darr,c+if(empty(j),pad(dieta,len(main->nr_zlec)-2),space(len(main->nr_zlec)-2))+skladnik+str(ilosc,8,3))} rest while dtos(data)+posilek=d
          endif
          next i
       endif
       if dflag
          j:=0
          *
          i:=1
          k:=len(main->nr_zlec)+len(surowce->skladnik)
          do while i<len(darr) //sumowanie
              j:=ascan(darr,{|x|left(darr[i],k)=left(x,k)},max(i+1,j))
              if j=0
                 ++i
              else
                 darr[i]:=left(darr[i],k)+str(val(subs(darr[i],k+1))+val(subs(darr[j],k+1)),8,3)
                 adel(darr,j)
                 asize(darr,len(darr)-1)
              endif
          enddo
          c:=len(main->nr_zlec)+1
          k:=len(surowce->skladnik)
          for i:=1 to len(darr)-2
              if i+1<(j:=ascan(darr,{|x|subs(darr[i],c,k)=subs(x,c,k)},max(i+1,++j)))
                 txt:=darr[j]
                 adel(darr,j)
                 ains(darr,i+1)
                 darr[i+1]:=txt
              endif
          next
          *
          for i:=1 to len(darr)
              message(10)
              j:=darr[i]
              SUROWCE->(dbseek(subs(j,c,k),.f.))
              darr[i]:=surowce->indx_mat+" "+left(j,c-1)+" "+left(surowce->nazwa,40)+HB_UTF8CHR(0x00A0)+subs(j,-8)+" "+surowce->jmag
          next
          if (dpos:=min(_flp+1,len(darr)))=1
             @ _frow+2,5 say 'Zapotrzebowanie na ten dzień pod [F8]' UNICODE color _sbkgr
          endif
          dflag:=dm->pozycja=D_LP0 .and. dpos>0
       endif
#else
    if empty(darr) .and. pm=-1 .and. dok$dok_di .and. dok_zew="W" .and. nr_mag=A_MAGDI //.and. ""#(txt:=getenv("DIETADEF"))
begin sequence
       if select("ro_zapot")=0
         select 0
         begin sequence
         if file("zapot.dbf")
#ifdef A_CDX
         nuse ("zapot") index ("zapot") readonly shared alias ro_zapot
         set order to tag zap_rel
#else
         nuse ("zapot") index ("zap_rel") readonly shared alias ro_zapot
#endif
         else
               sel("RO_ZAPOT","ZAP_REL",.t.)
         endif
         if dflag:=dbseek(dtos(da))
            select 0
            begin sequence
            if file("surowce.dbf")
#ifdef A_CDX
            nuse ("surowce") index ("surowce") readonly shared
            set order to tag sur_kod
#else
            nuse ("surowce") index ("sur_kod") readonly shared
#endif
            else
               sel("SUROWCE","SUR_KOD",.t.)
            endif
            recover
            use
            select ro_zapot
            dflag:=.f.
            break
            end sequence
         else
            break
         endif
         recover
         use
         end sequence
       else
         dflag:=ro_zapot->(dbseek(dtos(da)))
       endif
       if dflag
          select ro_zapot
          darr:={}
          j:=0
          mes:=message(hb_UTF8ToStr("Chwileczkę "))
//          exec {||message(10),aadd(darr,str(ascan(posilki,posilek)*15+ascan(diety,dieta),2)+str(recno(),6)+skladnik)} rest while data=da
          if fieldpos("POZYCJA")=0
             exec {||message(10),aadd(darr,{chr(ascan(posilki,posilek)+64),recno(),skladnik,dieta})} rest while data=da
          else
             exec {||message(10),aadd(darr,{chr(ascan(posilki,posilek)+64)+pozycja,recno(),skladnik,dieta})} rest while data=da
          endif
          asort(darr,,,{|a,b|a[1]<b[1]})

          for i:=1 to len(darr)-2
              if (j:=ascan(darr,{|x|darr[i,3]=x[3]},max(i+1,++j)))#0
              //{chr(ascan(posil,posilek)+64)+pozycja,recno(),skladnik+dieta}
                 txt:=darr[j]
                 adel(darr,j)
                 if txt[4]<darr[i,4]
                    --i
                 endif
                 ains(darr,i+1)
                 darr[i+1]:=txt
              endif
          next
          surowce->(dbgoto(0))
          j:=''
          k:=''
          i:=0
          do while i<len(darr)
              message(10)
              goto darr[++i,2]
#ifdef A_CIEZKO
              if surowce->skladnik==skladnik
                 --i
                 darr[i]:=surowce->indx_mat+" WS     "+surowce->nazwa+HB_UTF8CHR(0x00A0)+str(ilosc+val(subs(darr[i],-13)),8,3)+" "+surowce->jmag
                 adel(darr,i+1)
                 asize(darr,len(darr)-1)
              else
                 surowce->(dbseek(ro_zapot->skladnik,.f.))
                 darr[i]:=surowce->indx_mat+" WS     "+surowce->nazwa+HB_UTF8CHR(0x00A0)+str(ilosc,8,3)+" "+surowce->jmag
              endif
              j:=posilek
#else
#ifdef A_DODATKI
              if surowce->skladnik==skladnik .and. j==posilek
                 --i
                 darr[i]:=surowce->indx_mat+" W"+posilek+"     "+surowce->nazwa+HB_UTF8CHR(0x00A0)+str(ilosc+val(subs(darr[i],-13)),8,3)+" "+surowce->jmag
                 adel(darr,i+1)
                 asize(darr,len(darr)-1)
              else
                 surowce->(dbseek(ro_zapot->skladnik,.f.))
                 darr[i]:=surowce->indx_mat+" W"+posilek+"     "+surowce->nazwa+HB_UTF8CHR(0x00A0)+str(ilosc,8,3)+" "+surowce->jmag
              endif
              j:=posilek
#else
              if surowce->skladnik==skladnik .and. j==posilek .and. k==dieta
                 --i
                 darr[i]:=surowce->indx_mat+" "+pad("W"+posilek+dieta,len(main->nr_zlec))+" "+surowce->nazwa+HB_UTF8CHR(0x00A0)+str(ilosc+val(subs(darr[i],-13)),9,3)+" "+surowce->jmag
                 adel(darr,i+1)
                 asize(darr,len(darr)-1)
              else
                 surowce->(dbseek(ro_zapot->skladnik,.f.))
                 darr[i]:=surowce->indx_mat+" "+pad("W"+posilek+dieta,len(main->nr_zlec))+" "+surowce->nazwa+HB_UTF8CHR(0x00A0)+str(ilosc,9,3)+" "+surowce->jmag
              endif
              j:=posilek
              k:=dieta
#endif
#endif
          enddo
          if (dpos:=min(_flp+1,len(darr)))=1
             @ _frow+2,5 say 'Zapotrzebowanie na ten dzień pod [F8]' UNICODE color _sbkgr
          endif
          dflag:=dm->pozycja=D_LP0 .and. dpos>0
       endif
#endif //multidi
recover
      dflag:=.f.
      darr:={}
      dpos:=1
end sequence
       select dm
       message(mes)
    endif
#endif //dieta
#ifdef D_DIETA_OR_ODDO
    if !empty(darr)
       @ _frow+1,20 say 'F8' color _sbkgr
    else
       @ _frow+1,20 say '══' UNICODE color _sbkgr
    endif
#endif //dieta
return
***********************
proc dok4(_f,getlist,_s)
#ifdef A_SWW
   #define NIMBEG 4
   #define NIMLTH "6 "
#else
 #ifdef A_SHORTIND
   #define NIMBEG 7
   #define NIMLTH "4 "
 #else
   #ifdef A_KTM
      #define NIMBEG 2
      #define NIMLTH "16 "
   #else
      #define NIMBEG 3
      #ifdef A_OLZA
      #define NIMLTH "14 "
      #else
      #define NIMLTH "12 "
      #endif
    #endif
 #endif
#endif

#ifndef A_ZLEC11
#undef NIMBEG
#define NIMBEG 3
#endif

#ifdef A_MM
#undef NIMBEG
#define NIMBEG 5
#endif

#define NZBEG 25
#define NZPIC "@K!"


#ifdef A_OLZA
   #define NZVAL !" "$nz .or. gzle()
#else
   #ifdef A_OBR
      #undef  NZPIC
      #undef  NZBEG
      #define NZPIC "@KR XXXXXX"
      #define NZBEG 25
      #define NZVAL nzval(nz)
   #else
#ifdef A_ZLEC11
      #undef  NZBEG
      #define NZBEG 24
      #undef  NZPIC
#ifdef A_SB
      #define NZPIC "@KS" +ltrim(str(min(len(nr_zlec),21-NIMBEG-len( INDEXPIC ))))
#else
      #define NZPIC "@KS" +ltrim(str(min(len(nr_zlec),33-NIMBEG-len( INDEXPIC ))))
#endif
#endif
      #define NZVAL aczojs(stanowis[mag_poz])
#ifdef A_DIETA
      #undef NZVAL
#ifdef A_MULTIDI
      #define NZVAL aczojs(stanowis[mag_poz]).and.(mag_biez#A_MAGDI.or.!left(nz,1)$A_MULTIDI.or.dival())
#else
      #define NZVAL aczojs(stanowis[mag_poz]).and.(mag_biez#A_MAGDI.or.nz#'W'.or.dival())
#endif
#endif
   #endif
#endif

#undef NZBEG
#define NZBEG NIMBEG+len( INDEXPIC )+2

local txt,j,x,s:=0,si:='',gzl,y,z,a

      if iscolor()
        SET COLOR TO (_sbnorm)
      endif
      _fnowy:=KEY_DOK+nr_dowodu+pozycja#dm->(KEY_DOK+nr_dowodu)+D_LPPUT(_fi)
#ifndef STANY
      NOWYSTAN:=.t.
#endif
      nim:=space(46)
#ifdef A_NOMZ
      a:=len(nr_zlec)
#ifdef A_ZLEC11
      nz:=space(a)
#endif
#else
      a:=LEN(EvAlDb(IndexKey(3)))-10-len(index)
#endif
      il:=0
#ifdef A_WA
      chg_cen:=.f.
      wa:=ce:=ck:=0
#endif
#ifdef A_FA
      wz:=cz:=0
      pv:="  "
#endif
      if _fnowy

#ifdef A_MM
        _fpos:=2
#else
        _fpos:=1
#endif
        _fkey:=0
        lam:=NIL  //INDX_MAT->(select())
        
#ifdef A_SZYM
        if dok_p_r="F" .and. val(kh)#0
           nim:=pad(kh,46)
        endif
#endif
#ifdef A_F9
      if pflag .and. ppos#0
         j:=recno()
         goto ppos
         ppush:=.t.
         //y:=if(nr_mag+smb_dow$memvar->dok_rozch,-1,1)
         z:=MAX(1,ascan(dokumenty[MAG_POZ],smb_dow))
         y:=if(dok_par[MAG_POZ,z,1]="P",1,-1)
         parr:=array(8)
#ifdef A_SPECYF
         parr[8]:=KEY_DOK+nr_dowodu+pozycja
#endif
#ifdef A_MM
         parr[7]:=nr_mag
#endif
         parr[6]:=nr_zlec
         parr[1]:=index
#ifdef A_IZ
         if dok_par[MAG_POZ,z,8]='Z'
            parr[2]:=y*ilosc_f
            parr[5]:=y*cena
         else
            parr[2]:=y*if(ilosc=0,ilosc_f,ilosc)
#ifdef A_WA
            parr[5]:=y*wartosc
#endif
         endif
#else
         parr[2]:=y*ilosc
#ifdef A_WA
         parr[5]:=y*wartosc
#endif
#endif
         parr[3]:=ROUND(parr[5]/parr[2],A_ZAOKR)
#ifdef A_FA
   if dok_par[MAG_POZ,z,2]$"UV"
      parr[4]:=proc_vat
   else
      select indx_mat
      set order to tag INDX_NUM
#ifdef STANY
      dbseek(main->(nr_mag+index),.f.)
#else
      dbseek(main->index,.f.)
#endif
      parr[4]:=proc_vat
      select main
   endif
         if dok_par[MAG_POZ,z,1]="F"
            parr[3]:=cena
            parr[5]:=W(parr[2],parr[3],val(parr[4]),dok_df)
         endif
#endif
        nim:=pad("♫",46)
#ifdef A_MM
        mag_biez:=nr_mag
        //_fpos:=2
#endif
        txt:=KEY_DOK+nr_dowodu
        @ _fk,5 SAY "*** "+tran(index,"@R "+ INDEXPIC )+" "+str(parr[2])+" "+str(parr[3])+" ***"
        skip
        ppos:=if(KEY_DOK+nr_dowodu=txt,recno(),0)
        goto j
       endif
#endif
#ifdef D_DIETA_OR_ODDO
        if dpush:=(dflag .and. dpos<=len(darr))
           nim:=darr[dpos]
/*
#ifdef A_MM
           _fpos:=2
#endif
*/
#ifdef A_ODDO
         if dok$dok_zb
           il:=pm*val(subs(nim,len(index)+2))
#ifdef A_KASA
           nz:=subs(nim,44)
           pv:=subs(nim,20,2)
           cz:=val(subs(nim,24,10))
           wa:=pm*val(right(nim,10))
           if wa<>0
              ce:=ck:=wa/il
              chg_cen:=.t.
           endif
#ifdef A_CENSPEC
           if sp='CZ'
             ce:=ck:=&sp
             wa:=il*ce
             chg_cen:=.t.
           endif
#endif
           @ _fk,5 SAY subs(nim,9,46)
//#undef A_ODDO //po co to ?
#else //kasa
//#undef D_DIETA_OR_ODDO
           cz:=val(subs(nim,len(index)+21,10))
if dok_p_r="F"
           wz:=W(pm*il,cz,val(pv),dok_df)
else
           wz:=WPZ(pm*il*cz)
endif
           @ _fk,5 SAY subs(nim,len(index)+35,46)
           @ _fk+1,48 SAY subs(nim,len(index)+13,4)
           select STANY
           set order to 2
           dbseek(mag_biez+left(nim,len(index)),.f.)
#ifndef STANY
           select indx_mat
           set order to tag INDX_NUM
           dbseek(left(nim,len(index),.f.)
#endif
           lam:=i_lam(da)
           pv:=(lam)->proc_vat
           @ _fk+1,48 say (lam)->jm
           select main
           NIM:=padr(INDX_MAT->INDEX,46)
#endif //kasa
         endif
#endif //oddo
#ifdef A_DIETA //#else  //oddo
         if dok$dok_di
           il:=-val(subs(nim,rat(HB_UTF8CHR(0x00A0),nim)+1))
           nz:=subs(nim,len(index)+2,len(nr_zlec))
           @ _fk+1,48 say right(nim,4)
           txt:=subs(nim,len(INDEX)+2+LEN(nr_zlec)+1)
           j:=rat(HB_UTF8CHR(0x00A0),txt)
           txt:=left(nim,len(INDEX)+2)+trim(nz)+' '+TRIM(left(txt,j-1))+' '+subs(txt,j+1)
           @ _fk+2,1 SAY "ZAPOTRZEBOWANIE: "+txt+space(60-len(txt)) color _sbkgr
           nim:=pad(left(nim,len(index)),46)
           select STANY
           set order to 2
           dbseek(mag_biez+left(nim,len(index)),.f.)
#ifndef STANY
           select indx_mat
           set order to tag INDX_NUM
           dbseek(left(nim,len(index)),.f.)
#endif
           select main

         endif
#endif //a_dieta
           ++dpos
         endif
#endif //d_dieta_or_oddo
***********
      else  //_fnowy
***********
#ifdef A_LAN
/* bo w ebl a naglowek i tak zablokowany
        if _fkey=0
           LOCK
        endif
*/
#endif
#ifndef STANY
        NOWYSTAN:=STANY->(DBSEEK(MAIN->(NR_MAG+INDEX),.F.))
        indx_mat->(dbseek(main->(index),.f.))
#else
        STANY->(DBSEEK(MAIN->(NR_MAG+INDEX),.F.))
#endif
        lam:=i_lam(dm->data)
#ifdef A_MM
        mag_biez:=nr_mag
#endif
        nim:=pad(index,46)
        nz:=nr_zlec
#ifdef A_IZ
        il:=if(dok_ew="E",ilosc,ilosc_f)
#else
        il:=ilosc
#endif
#ifdef A_FA
        if dok_zew$"UV"
           @ _fk,50 say "%" COLOR _sbkgr
           pv:=proc_VAT
        else
           pv:=(lam)->proc_vat
        endif
        cz:=cena
        if dok_p_r="F"
           wz:=W(pm*il,cz,val(pv),dok_df)
        else
           wz:=pm*cz
           cz:=if(il=0,0,ROUND(cz/il,A_ZAOKR))
        endif
#endif
#ifdef A_WA
        wa:=wartosc
#endif
#ifdef A_JMO
        @ _fk+1,48 say (lam)->(if(miar_opcja,jm_opcja,jm))
#else
        @ _fk+1,48 say (lam)->jm
#endif
        @ _fk,5 SAY (lam)->nazwA
#ifdef A_IZ
        if dok_ew#"Z"
#endif
#ifdef A_WA
        ce:=ROUND(ck:=if(il=0,0,wa/il),A_ZAOKR)
        if !chg_cen .and. STANY->(found())
           ck:=STANY->(if(stan=il,CENA_PRZY,MAX(0,(WARTOSC-wa)/(STAN-il))))
#ifdef A_FIFO
           if _fkey=0
              getck(_fnowy)
              UNLOCK IN STANY
           endif
#endif
           chg_cen:=ROUND(ROUND(il*ck,A_ZAOKR)-wa,A_ZAOKR)#0
        endif
#endif


#ifndef wtoT
        if _fi>posproc
#ifdef A_WA
           wtot+=wa
#else
           wtot+=ilosc*(lam)->cenA
#endif
        endif
#endif

#ifdef A_IZ
      endif
#endif
        if _fi>posproc
           posproc:=_fi
#ifdef A_GRAM
            gtot+=pm*il*(lam)->gram/1000
#endif
#ifdef A_JMTOT
#ifdef A_JMALTTOT
           j:=ascan(itot,{|x|x[1]=(lam)->jm_opcja})
           if j=0
              aadd(itot,{(lam)->jm_opcja,A_JMALTTOT(pm*il,nz,lam,x)})
           else
              itot[j,2]+=A_JMALTTOT(pm*il,nz,lam,x)
           endif
#endif
           j:=ascan(itot,{|x|x[1]=(lam)->D_JM})
           if j=0
              aadd(itot,{(lam)->D_JM,pm*il})
           else
              itot[j,2]+=pm*il
           endif
#endif
        endif

      endif
#ifdef A_MM
        @ _fk+1, NIMBEG-3 GET mag_biez PICTURE "@K XX";
         VALID {|g,x,y|aczojs(magazyny).and.(g:original=mag_biez .or. (nim:=space(46),.t.)).and.(g:ExitState=GE_DOWN .or. g:ExitState=GE_ENTER .or. pomval(g,_f,y,_s))}
#endif
#ifdef A_ANKER
        @ _fk+1, NIMBEG SAY ean13(nim)
#endif

        txt:=getnew(_fk+1,NIMBEG,{|x|if(pcount()=0,nim,nim:=x)},"nim","@!KRS"+NIMLTH+strtran( INDEXPIC ,"#","X")+repl("X",46-len(index)))
        txt:display()
      if _fkey=0
#ifdef A_F9
        txt:reader:={|g|setkey(K_F9,{|p,g|g:=getactive(),p:=setkey(K_F9,NIL),pchoice(g,getlist),setkey(K_F9,p)}),getreader(g),setkey(K_F9,NIL)}
        txt:postblock:={|g,k,z,y|k:=setkey(K_F9,NIL),y:=pomval(g,_f,z,_s),setkey(K_F9,k),y}
#else
        txt:postblock:={|g,x,y|pomval(g,_f,y,_s)}
#endif
#ifdef A_LAN
#ifdef A_IZ
        if dok_ew#"Z"
#endif
         __SetEditBlock({|g,gl|ebl(g,_f,gl)} )
         txt:cargo:={|g,gl|ebl(g,_f,gl)}
         /*
#ifdef A_MM
         getlist[1]:cargo:={|g|ebl(g,_f,getlist)}
#endif
         */
#ifdef A_IZ
        endif
#endif
#endif
#ifdef D_DIETA_OR_ODDO
         if !empty(darr)
            txt:reader:={|g|setkey(K_F8,{|p,g|g:=getactive(),p:=setkey(K_F8,NIL),azapchoice(g,_f,getlist),setkey(K_F8,p)}),getreader(g),setkey(K_F8,NIL)}
            txt:postblock:={|g,k,z,y|k:=setkey(K_F8,NIL),y:=pomval(g,_f,z,_s),setkey(K_F8,k),y}
/*
#ifdef A_ODDO
        if dok$DOK_ZB
            setkey(K_F8,{|p|oddochoice(p,_f,getlist)})
        endif
#endif
*/
         endif
#endif
        aadd(getlist,txt)
      endif
#ifdef A_FA
#ifndef A_RODZ_S
#define A_RODZ_S
#endif
#endif
#ifdef A_RODZ_S
#define zaMowienie if(pm=1,zamowienie,rodz_sprzed)
#endif
      if dok_p_r="S"
           if !_fnowy // .and. _fkey=0
              s:=stanprzeD(_fnowy,@si,s)
              si:=STANY->(nr_mag+index)
           endif
#ifdef A_JMO
           if miar_opcja
             txt:=getnew(_fk+1,24,{|x|s:=stanprzeD(_fnowy,si,s),si:=STANY->(nr_mag+index),if(pcount()=0,(x:=s+il,if(x=0,0,int(x/(lam)->przel)+(x%(lam)->przel)/1000)),(x:=int(x)*(lam)->przel+(x%1*1000),il:=ROUND(x-s,ILDEC)))},"il","@K #####r.###")
           else
#endif
             txt:=getnew(_fk+1,24,{|x|s:=stanprzeD(_fnowy,si,s),si:=STANY->(nr_mag+index),if(pcount()=0,S+il,il:=ROUND(x-S,ILDEC))},"il","@K "+ILPIC)
#ifdef A_JMO
           endif
#endif
           txt:display()

           if _fkey=0
             txt:postblock:={|g|!g:changed.or.(getlist[gil]:display(),groz(g,_f,getlist))}
             aadd(getlist,txt)
             gzl:=len(getlist)
           endif

      endif
      j:=NIL
      do case
        case ""=dok_kon
        case dok_kon="&:"
        case dok_kon="*"
          j:=getnew(_fk+1,NZBEG,{|x|if(x=NIL,nz,nz:=x)},"nz",NZPIC)
          j:postblock:=if(dok_zew#"W",;
{||if(empty(nz),aczojs(zaMowienie[MAG_POZ]),nz:=UpP(nz)),.t.},;
{||if(empty(nz),NZVAL,nz:=UpP(nz)),.t.})
        case isdigit(dok_kon) .or. isalpha(dok_kon)
            if !_fnowy
#ifndef A_NOMZ
            elseif dok_kh
              nz:=pad(dok_kon,a-A_NRLTH)+kh
#endif
            else
              nz:=pad(dok_kon,a)
            endif
#ifndef A_OBR
            // plus
        case len(txt:=if(dok_zew="W",stanowis,zaMowienie)[mag_poz])=1 .and.! "*" $ left(txt[1],a)
            if _fnowy
               nz:=left(txt[1],a)
#ifndef A_NOMZ
               if dok_zew#"W" .and. (dok_kon#"?" .or. !empty(kh)) .and. empty(subs(nz,a-A_NRLTH+1,A_NRLTH))
                 nz:=left(nz,a-A_NRLTH)+kh
               endif
#endif
            endif
#endif
        case dok_zew#"W"
            j:=getnew(_fk+1,NZBEG,{|x|if(x=NIL,nz,nz:=x)},"nz",NZPIC)
#ifdef A_NOMZ
            j:postblock:={||aczojs(zaMowienie[MAG_POZ])}
#else
#ifdef A_KOBSUR
            j:postblock:=if(dok_kon="?".and.empty(kh),{||aczojs(zaMowienie[MAG_POZ])},{|g,x,y|y:=.t.,x:=nz,aczojs(zaMowienie[MAG_POZ],@x) .AND.;
                  (if(empty(subs(x,a-A_NRLTH+1,A_NRLTH)),;
                  (nz#(nz:=left(x,a-A_NRLTH)+kh).and.updated(@y)),;
                  (nz:=x)#g:original.and.updated(@y)),y) .and. nzval(g,_f,getlist)})
#else
            j:postblock:=if(dok_kon="?".and.empty(kh),{||aczojs(zaMowienie[MAG_POZ])},{|g,x,y|y:=.t.,x:=nz,aczojs(zaMowienie[MAG_POZ],@x) .AND.;
                  (if(empty(subs(x,a+1-A_NRLTH,A_NRLTH)),;
                  (nz#(nz:=padr(left(x,a-A_NRLTH)+kh,len(NR_ZLEC))).and.updated(@y)),;
                  (nz:=x)#g:original.and.updated(@y)),y)})
#endif
#endif
        otherwise
            j:=getnew(_fk+1,NZBEG,{|x|if(x=NIL,nz,nz:=x)},"nz",NZPIC)
            j:postblock:={||NZVAL}
      endcase
      if dok_p_r#"S"
            if j#NIL
               aadd(getlist,j)
               j:display()
            else
               @ _fk+1,NZBEG say nz PICTURE NZPIC color _sbkgr
            endif
       endif
#ifdef A_FA
#undef zaMowienie
#endif
#ifdef A_JMO
      if miar_opcja
        txt:=getnew(_fk+1,36,{|x|if(pcount()=0,if(il=0,0,int(pm*il/(lam)->przel)+(pm*il%(lam)->przel)/1000),il:=ROUND(pm*(int(x)*(lam)->przel+(x%1*1000)),ILDEC))},"il","@K #####r.###")
      else
#endif
        txt:=getnew(_fk+1,36,{|x|if(pcount()=0,pm*il,il:=pm*x)},"il","@K "+ILPIC)
#ifdef A_JMO
      endif
#endif


      if dok_p_r="S"
      txt:postblock:={|g|!g:changed.or.(getlist[gzl]:display(),groz(g,_f,getlist))}
      else
      txt:postblock:={|g|!g:changed.or.groz(g,_f,getlist)}
      endif
      aadd(getlist,txt)
      gil:=len(getlist) // pozycja il w getlist
#ifdef A_SB
      if dok$dok_di
         if !_fnowy //.and. _fkey=0
            s:=stanprzeD(_fnowy,@si,s)
            si:=STANY->(nr_mag+index)
         endif
#ifdef A_JMO
           if miar_opcja
             j:=getnew(_fk+1,36,{|x|s:=stanprzeD(_fnowy,si,s),si:=STANY->(nr_mag+index),if(pcount()=0,(x:=s+il,if(x=0,0,int(x/(lam)->przel)+(x%(lam)->przel)/1000)),(x:=int(x)*(lam)->przel+(x%1*1000),il:=ROUND(x-s,ILDEC)))},"il","@K #####r.###")
           else
#endif
             j:=getnew(_fk+1,36,{|x|s:=stanprzeD(_fnowy,si,s),si:=STANY->(nr_mag+index),if(pcount()=0,S+il,il:=ROUND(x-S,ILDEC))},"il","@K "+ILPIC)
#ifdef A_JMO
           endif
#endif
#ifdef A_SBSHOW
         j:preblock:={||.f.}
#endif
         txt:postblock:={|g|!g:changed.or.(getlist[gil+1]:display(),groz(g,_f,getlist))}
         txt:col:=24
         j:display()

         j:postblock:={|g|!g:changed.or.(getlist[gil]:display(),groz(g,_f,getlist))}
         aadd(getlist,j)
      endif
#endif
      txt:display()
      if _fkey=0
#ifdef A_SPECYF
         setkey(92,{|p,q,r|q:=getactive(),q:name='nim'.or.(p:=setkey(92,NIL),r:=specyfik(_f,,q), r=0 .or. (il:=pm*r,groz(q,_f,getlist)),setkey(92,p),.t.)})
#endif
#ifdef A_FA
   if dok_p_r="F"
         if dok_war="-"
            txt:reader:={|g|setkey(61,{|p|add_faget(_f,getlist),setkey(61,NIL)}),getreader(g),setkey(61,NIL)}
         else
            add_faget(_f,getlist)
#ifdef A_WA
            if dok_war="+"
               atail(getlist):reader:=NIL
               add_get(_f,getlist)
            endif
#endif
         endif
   elseif dok_ew#"E"
#ifdef A_HLINK
         HLINK:=NIL
#endif
#ifndef A_NOPV
         if dok_zew$"UV"
         @ _fk,49 get pv picture "@K XX" valid {||pv:=lower(pv),if(pv<="9",pv:=str(val(pv),2),),(ascan(stawki,pv)#0 .or. alarm('NIEWŁAŚCIWY PROCENT VAT!',,3,3)=NIL) .and. showvat(_f)}
         endif
#endif
         j:=getnew(_fk,53,{|x|if(pcount()=0,cz,cz:=x)},"cz",WAPIC)
         j:display()
         aadd(getlist,j)
         txt:=getnew(_fk,66,{|x|if(pcount()=0,wz,wz:=x)},"wz",WAPIC)
         txt:display()
         aadd(getlist,txt)
#ifdef A_IZ
         if dok_ew="Z"
           j:postblock:={|g|!g:changed.or.(wz:=WPZ(pm*il*cz),showvat(_f))}
           txt:postblock:={|g|!g:changed.or.(wz:=WPZ(wz),cz:=if(il=0,0,ROUND(pm*wz/il,A_ZAOKR)),showvat(_f))}
         else
#endif
#ifdef A_WEBRUT
//#ifndef A_NOPV
           getlist[len(getlist)-2]:postblock:={|g|pv:=lower(pv),if(pv<="9",pv:=str(val(pv),2),),(ascan(stawki,pv)#0 .or. alarm('NIEWŁAŚCIWY PROCENT VAT!',,3,3)=NIL) .and. (!g:changed .or. cenpz(_f,getlist)) }
//#endif
#endif
           j:postblock:={|g|!g:changed.or.(wz:=WPZ(pm*il*cz),cenpz(_f,getlist))}
           txt:postblock:={|g|!g:changed.or.(wz:=WPZ(wz),cz:=if(il=0,0,ROUND(pm*wz/il,A_ZAOKR)),cenpz(_f,getlist))}
#ifdef A_IZ
        endif
        if dok_ew#"Z"
#endif
#ifdef A_WA
 #ifndef cenA_zaK
  #ifdef A_WEBRUT
         if dok_p_r="P" .and. dok_war="+"
  #else
         if dok_p_r="P" .and. ( dok_war="+" .or. wa<>wz )
  #endif
            add_get(_f,getlist)
         else
            txt:reader:={|g|setkey(61,{|p|add_get(_f,getlist),setkey(61,NIL)}),getreader(g),setkey(61,NIL)}
         endif
 #endif
#else
         if dok_war="-"
            txt:reader:={|g|setkey(61,{||dok_w1:="+",showwar(_f,getlist,gil),setkey(61,NIL)}),getreader(g),setkey(61,NIL)}
         endif
#endif
#ifdef A_IZ
        endif
#endif
#ifdef cenA_zaK
        if /*dok_df .and.*/ dok_zew$"UV"

  colorselect(3)
           x:=getnew(_fk+1,53,{|x,v|v:=val(pv),if(pcount()=0,,(x:=Round(x*il,A_ZAOKR),wz:=x-ROUND(x*v/(100+v),A_ZAOKR))),ROUND((wz+VATPZ(wz,v))/il,A_ZAOKR)},"wz",WAPIC)
           x:postblock:=txt:postblock
           x:display()
           aadd(getlist,x)

           y:=getnew(_fk+1,66,{|x,v|v:=val(pv),if(pcount()=0,,wz:=x-ROUND(x*v/(100+v),A_ZAOKR)),wz+VATPZ(wz,v)},"wz",WAPIC)
           y:postblock:=txt:postblock
           y:display()
           aadd(getlist,y)
  colorselect(0)
        endif
#endif
   else
#endif
#ifdef A_WA
      if dok_war="+"
          add_get(_f,getlist)
      else
          txt:reader:={|g|setkey(61,{|p|add_get(_f,getlist),setkey(61,NIL)}),getreader(g),setkey(61,NIL)}
      endif
#else
      if dok_war="-"
         txt:reader:={|g|setkey(61,{||dok_w1:="+",showwar(_f,getlist,gil),setkey(61,NIL)}),getreader(g),setkey(61,NIL)}
      endif
#endif
#ifdef A_FA
   endif
#endif
#ifndef A_WA
      dok_w1:=dok_war
#endif
      __setproc(procname(0))
      setkey(402,{|p,g|g:=getactive(),MAIN->(doinsline(_f,getlist,g,p))})
      else
        inkey()
      endif //_fkey=0

      showwar(_f,getlist,gil)
      UNLOCK IN STANY
      UNLOCK
return
***********************
stat proc doinsline(_f,getlist,g,p)
local a:={}

IF p='DOK4'
  IF _fnowy .or. updated(NIL) .or. valtype(g)#'O'
   tone(130,3)
  ELSE
   LOCK IN DM
   DM->POZYCJA:=D_LPPUT(D_LPVAL(DM->pozycja)+1)
#ifdef A_LAN
#define D_LAN {||reclock(.F.,"NIE POTRAFIĘ ZMIENIĆ NUMERU W POZYCJI"+D_LPSTR(POZYCJA),,,recno())}
#define D_LAN0 ,dbrunlock(r)
#else
#define D_LAN
#define D_LAN0
#endif
   dbeval({||aadd(a,recno())},D_LAN,{||KEY_DOK+nr_dowodu=dm->(KEY_DOK+nr_dowodu)})
   AEVAL(a,{|r,i|dbgoto(r),MAIN->pozycja:=D_LPPUT(_fi+i) D_LAN0})
#undef D_LAN
#undef D_LAN0
   append blank
   nr_mag:=mag_biez
   smb_dow:=dm->smb_dow
   nr_dowodu:=dm->nr_dowodu
   pozycja:=D_LPPUT(_fi)
   data:=dm->data
   unlock
   ++posproc
   ++_flp
   if _fskip*(_fl-_fj+2)+_frow>maxrow()
      if _fl=_flp-1
         @ _fskip*(_fl-_fj+1)+_frow,_fco1 SAY  '╚'+replicate('─',_fco2-_fco1-1)+'╝' color _sbkgr
      endif
   else
      ++_fl
      @ (_fl-_fj)*_fskip+_frow,_fco1,(_fl-_fj+1)*_fskip+_frow,_fco2 BOX UNICODE '║ ║║╝═╚║ ' color _sbkgr
      @ (_fl-_fj)*_fskip+_frow,_fco1+1 SAY str(_fl,3)+'.' color _sbkgr
   endif
   g:killfocus()
   a:=savescreen(_fk,_fco1+5,_fk+1,_fco2-1)
   scroll(_fk,_fco1+5,_frow+_fskip*(_fl-_fj+1)-1,_fco2-1,-_fskip)
   restscreen(_fk,_fco1+5,_fk+1,_fco2-1,a)
   g:exitState:=GE_TOP
   INDX_MAT->(dbgoto(lastrec()+1))
   g:setfocus()
   updated(.t.)
  ENDIF
ELSEIF procname(3)='SZUKAM' .and. !_fnowy
   a:=message("Ilość:             "+indx_mat->jm)
   p:=0
   getreader(g:=getnew(a[1]+1,a[2]+8,{|x|if(pcount()=0,p,p:=x)},"",WAPIC))
   message(a)
   if g:exitstate=GE_ENTER .and. p#0
      LOCK IN DM
      DM->POZYCJA:=D_LPPUT(_flp:=D_LPVAL(DM->pozycja)+1)
      a:=recno()
      append blank
      nr_mag:=mag_biez
      smb_dow:=dm->smb_dow
      nr_dowodu:=dm->nr_dowodu
      pozycja:=DM->pozycja
      data:=dm->data
      index:=INDX_MAT->index
#ifdef A_IZ
      if dok_ew='Z'
         ilosc_f:=p*pm
      else
         ilosc_f:=p*pm
#endif
         ilosc:=p*pm
         SELECT STANY
         LOCK recno()
#ifndef STANY
#ifdef A_ZAGRODA
         IF INDX_MAT->zapas_id<>0
           STANY->unlink:=.t.
         ENDIF
#endif
#endif
         stan:=round(stan+ilosc,3)
         UNLOCK recno()
         SELECT MAIN
#ifdef A_IZ
      endif
#endif
         goto a
   endif
ELSE
   tone(130,3)
ENDIF
   UNLOCK IN STANY
   UNLOCK IN MAIN
return
***********************
func stanprzed(nowy,si,s,w)
local a:=0,b:=0,sel,rcrd
if si#STANY->(nr_mag+index)
   s:=STANY->STAN
#ifdef A_WA
   w:=STANY->WARTOSC
#endif
   si:=STANY->(nr_mag+index)
   sel:=select()
   select main
   rcrd:=recno()
   set order to tag MAIN_IND
   if !nowy
      a:=ilosc
#ifdef A_WA
      b:=wartosc
#endif
   endif
   seek si+dtos(da)+'@'
#ifdef A_WA
   exec {||a+=ilosc,b+=wartosc} rest while nr_mag+index=si
#else
   exec {||a+=ilosc} rest while nr_mag+index=si
#endif
   go rcrd
   set order to tag MAIN_NRK
   select (sel)
   s:=s-a
#ifdef A_WA
   w:=w-b
#endif
endif
return s
***********************
#ifdef A_LAN
func EBL(g,_f,getlist,nock)
local pass:=dbrlocklist()
LOCK
if mag_biez=STANY->nr_mag .and. nim=indx_mat->index .and. !indx_mat->(eof())
lam:=i_lam(dm->data)
begin sequence
#ifndef STANY
     pass:=.f. // nowystan itp
     do while .t.
        if STANY->(DBSEEK(MAG_BIEZ+indx_mat->INDEX,.F.))
#endif
           LOCK IN STANY
#ifndef STANY
           if STANY->(deleted())
              UNLOCK IN STANY
              loop
           endif
#endif
#ifdef A_WA
           if nock=NIL // w peoma
           if _fnowy
              ck:=STANY->(if(stan=0,CENA_PRZY,MAX(0,WARTOSC/STAN)))
           else
              private il:=ilosc,wa:=wartosc
#ifdef A_FIFO
              getck(_fnowy)
#else
              ck:=if(round(STANY->stan-ilosc,3)=0,STANY->CENA_PRZY,MAX(0,(STANY->WARTOSC-wartosc)/(STANY->STAN-ilosc)))
#endif
              if chg_cen#(chg_cen:=ROUND(wartosc-ROUND(ilosc*ck,A_ZAOKR),A_ZAOKR)#0.or.ilosc=0.and.wartosc#0)
                 showwar(_f,getlist,gil)
              endif
           endif
           endif
#endif
#ifndef STANY
           NOWYSTAN:=.F.
           if pass
              unlock in indx_mat
           endif
           UNLOCK IN STANY
           exit
        ELSE
           LOCK IN indx_mat
           if indx_mat->(deleted())
              recall
           endif
           lam:=i_lam(dm->data)
           @ _fk,5 SAY (lam)->nazwA
#ifdef A_JMO
           @ _fk+1,48 say (lam)->(if(miar_opcja,jm_opcja,jm))
#else
           @ _fk+1,48 say (lam)->jm
#endif
           if pass
              unlock in indx_mat
              exit
           endif
           pass:=NOWYSTAN:=.T.
        endif
     enddo
#endif
recover
        return .f. // undo
end sequence
return NIL // kasuj ebl
endif
return .t. // nie kasuj ebl - bo i tak zmiana indeksu
#endif
/***********************
#ifdef A_ODDO
static proc oddochoice(p,_f,getlist)
local pos:=dpos,x
       if p="DOK4" .and. (dflag:=aczojs(darr,@nim,@pos,,"Zbiorówka:"))
          dpos:=pos
          nim:=darr[dpos]
          @ _fk,5 SAY subs(nim,len(index)+35,46) color _sbkgr
          il:=pm*val(subs(nim,len(index)+2))
          cz:=val(subs(nim,len(index)+21))
          NIM:=left(nim,len(index))
          select STANY
          set order to 2
          dbseek(mag_biez+nim,.f.)
#ifndef STANY
          select indx_mat
          set order to tag INDX_NUM
          dbseek(nim,.f.)
#endif
          lam:=i_lam(da)
          updated(.t.)
          select main
          pv:=(lam)->proc_vat
if dok_p_r="F"
          wz:=W(pm*il,cz,val(pv),dok_df)
else
          wz:=WPZ(pm*il*cz)
endif
          @ _fk+1,48 say (lam)->jm
          NIM:=padr(INDX_MAT->INDEX,46)
          aeval(getlist,{|x|x:display()})
          showwar(_f,getlist,gil)
          ++dpos
       endif
return
#endif
****************************/
#ifdef A_F9
static proc pchoice(g,getlist)
local stat:=push_stat(),txt,x,y,r

if pflag:=dmprzeg(ppos)
   select main
   ppush:=.t.
   ppos:=recno()
   r:=MAX(1,ascan(dokumenty[MAG_POZ],smb_dow))
   y:=if(dok_p_r="P",1,-1)
   parr:=array(8) //index,ilosc,cena,vat,wartosc
#ifdef A_SPECYF
   parr[8]:=KEY_DOK+nr_dowodu+pozycja
#endif
#ifdef A_MM
   parr[7]:=nr_mag
#endif
   parr[6]:=nr_zlec
   parr[1]:=index

#ifdef A_IZ
   if dok_ew='Z'
            parr[2]:=y*ilosc_f
            parr[5]:=y*cena
   else
            parr[2]:=y*ilosc
#ifdef A_WA
            parr[5]:=y*wartosc
#endif
   endif
#else
   parr[2]:=y*ilosc
#ifdef A_WA
   parr[5]:=y*wartosc
#endif
#endif

   parr[3]:=ROUND(parr[5]/parr[2],A_ZAOKR)
#ifdef A_FA
   if dok_zew$"UV"
      parr[4]:=proc_vat
   else
      select indx_mat
      set order to tag INDX_NUM
#ifdef STANY
      dbseek(main->(nr_mag+index),.f.)
#else
      dbseek(main->index,.f.)
#endif
      parr[4]:=proc_vat
      select main
   endif
   if dok_p_r="F"
      parr[3]:=cena
      parr[5]:=W(parr[2],parr[3],val(parr[4]),dok_df)
   endif
#endif
   nim:=pad("♫",46)
   txt:=KEY_DOK+nr_dowodu
   skip
   ppos:=if(KEY_DOK+nr_dowodu=txt,recno(),0)
else
   ppush:=.f.
endif
pop_stat(stat)
return
#endif

#ifdef D_DIETA_OR_ODDO
static proc azapchoice(g,_f,getlist)
  local pos:=dpos,txt,p,x
       setpos(row(),45)

#ifdef A_KSEF
/*
&:__mvPrivate("_bv,_bp,_bva,_bvi,_ap,_sb0,_sb1,_sb")
&:_bv:={|g,a|a:=eval(g:block),g:=g:subscript,a:=a[g[1]],a[6]:=round(a[5]*a[4],2),a[8]:=round(a[6]*(a[7]+100)/100,2),.t.}
&:_bp:={|x|x[1]:=if(Empty(x[1]),'      ',x[1]),x[2]:=if(Empty(x[2]),'    ',x[2]),x[4]:=if(empty(x[4]),0,val(strtran(x[4],',','.'))),x[5]:=if(empty(x[5]),0,val(strtran(x[5],',','.'))),x[7]:=if(empty(x[7]),0,val(x[7])),x[6]:=round(x[5]*x[4],2),x[8]:=round(x[6]*(x[7]+100)/100,2),indx_mat->(dbseek(mag_biez+left(x[2],4),.f.)),x[9]:=left(indx_mat->nazwa,30)}
&:_bva:={|g,a,r,x|a:=eval(g:block),r:=g:subscript,a:=a[r[1]],a[2]:=indx_mat->index,a[9]:=left(indx_mat->nazwa,16),a[7]:=val(indx_mat->proc_vat),a[3]:=indx_mat->jm,a[5]:=indx_mat->cena_zak,eval(_bv,g)}
&:_bvi:={|g,v,r,a,s|if(empty(subs(v,9)),(s:=push_stat(),a:=array(11),a[10]:=UPPER(Trim(v)),a[11]:=.t.,r:=katalog(a,g),if(r,eval(_bva,g),),pop_stat(s),r),.t.)}
&:_ap:={'@K',,,'@KZ ######.###','@EKZ ####.##','@EZ #####.##','@KZ ##','@EZ ######.##'}
&:_sb0:={|y,z|_txt:=_txt+AllTrim(Tran(y,_ap[z]))+';'}
&:_sb1:={|x|aeval(x,_sb0),_txt:=_txt+chr(13)+chr(10)}
&:_sb:={|g,a|_txt:='',aeval(a,_sb1),g:varput(Left(_txt,Len(_txt)-3))}
{|a,g,r,k|a:=make_subar(getlines(g:varget()),9),aeval(a,_bp),k:=setkey(-41,NIL),r:=arredit(a,{'Pakiet','Surowce','J.M.','Ilość','Cena','Wartość','%V','Brutto'},_ap,{,_bvi,,_bv,_bv},{,,.f.,,,.f.,.f.,.f.,.f.},2,8,{|s,a,i|_p:=Trim(a[i,1]),_s:=0,aeval(a,{|x|if(Trim(x[1])==_p,_s+=x[8],)}),_s}),setkey(-41,k),if(r,eval(_sb,g,a),)
*/
       if (dflag:=aczojs(darr,@nim,@pos,,"Zbiorówka "+str(len(darr),3)+" pozycji"))
#else
       if (dflag:=aczojs(darr,@nim,@pos,,"Zbiorówka "+str(len(darr),3)+" pozycji"))
#endif
          dpos:=pos
          nim:=darr[dpos]
#ifdef A_ODDO
          if dok$dok_zb
          @ _fk,5 SAY subs(nim,len(index)+35,46) color _sbkgr
          il:=pm*val(subs(nim,len(index)+2))
          cz:=val(subs(nim,len(index)+21))
          NIM:=left(nim,len(index))
          select STANY
          set order to 2
          dbseek(mag_biez+nim,.f.)
#ifndef STANY
          select indx_mat
          set order to tag INDX_NUM
          dbseek(nim,.f.)
#endif
          lam:=i_lam(da)
          updated(.t.)
          select main
          pv:=(lam)->proc_vat
if dok_p_r="F"
          wz:=W(pm*il,cz,val(pv),dok_df)
else
          wz:=WPZ(pm*il*cz)
endif
          @ _fk+1,48 say (lam)->jm
          NIM:=padr(INDX_MAT->INDEX,46)
          aeval(getlist,{|x|x:display()})
          showwar(_f,getlist,gil)
       endif
#endif
#ifdef A_DIETA
       if dok$dok_di
          dpush:=.t.
#ifdef A_KASA
          @ _fk,5 SAY subs(nim,len(index)+2,46)
#else
          il:=-val(subs(nim,rat(HB_UTF8CHR(0x00A0),nim)+1))
          nz:=subs(nim,len(index)+2,len(main->nr_zlec))
          txt:=subs(nim,len(INDEX)+len(main->nr_zlec)+3)
          p:=rat(HB_UTF8CHR(0x00A0),txt)
          txt:=left(nim,len(INDEX)+2)+trim(nz)+' '+trim(left(txt,p-1))+subs(txt,p)
          if _fi>=_flp
             @ _fk+2,1 SAY "ZAPOTRZEBOWANIE: "+txt+space(60-len(txt)) color _sbkgr
          else
             @ _fk,7 SAY "ZAPOTRZEB: "+txt+space(60-len(txt)) color _sbkgr
          endif
#endif
       endif
#endif
       ++dpos
       endif
return
#endif
***********************
proc dok41(_f)
local txt,j,x,s
      _fnowy:=.f.
      if _flp=0
         return
      endif
      if iscolor()
        SET COLOR TO (_sbnorm)
      endif
#ifdef STANY
      indx_mat->(dbseek(main->(nr_mag+index),.f.))
#else
      indx_mat->(dbseek(main->(index),.f.))
#endif
      lam:=i_lam(dm->data)
      @ _fk,5 SAY (lam)->nazwA
#ifdef A_JMO
      @ _fk+1,48 say (lam)->(if(miar_opcja,jm_opcja,jm))
#else
      @ _fk+1,48 say (lam)->jm
#endif
#ifdef A_IZ
      il:=if(dok_ew="E",ilosc,ilosc_f)
#else
      il:=ilosc
#endif
#ifdef A_WA
      wa:=wartosc
      ce:=IF(IL=0,0,ROUND(wa/il,A_ZAOKR))
#endif
#ifdef A_FA
      if dok_zew$"UV"
         @ _fk,50 say "%" COLOR _sbkgr
         pv:=proc_vat
      else
         pv:=(lam)->proc_vat
      endif
      cz:=cena
      if dok_p_r="F"
        wz:=W(pm*il,cz,val(pv),dok_df)
      else
        wz:=pm*cz
        cz:=if(il=0,0,ROUND(cz/il,A_ZAOKR))
      endif
#endif
#ifdef A_IZ
      if dok_ew#"Z"
#endif
#ifndef wtoT
      if _fi>posproc
#ifdef A_WA
         wtot+=wa
#else
         wtot+=ilosc*(lam)->cenA
#endif
      endif
#endif
#ifdef A_IZ
      endif
#endif
      if _fi>posproc
         posproc:=_fi
#ifdef A_GRAM
      gtot+=pm*il*(lam)->gram/1000
#endif
#ifdef A_JMTOT
#ifdef A_JMALTTOT
           j:=ascan(itot,{|x|x[1]=(lam)->jm_opcja})
           if j=0
              aadd(itot,{(lam)->jm_opcja,A_JMALTTOT(pm*il,nr_zlec,lam,x)})
           else
              itot[j,2]+=A_JMALTTOT(pm*il,nr_zlec,lam,x)
           endif
#endif
         j:=ascan(itot,{|x|x[1]=(lam)->D_JM})
         if j=0
              aadd(itot,{(lam)->D_JM,pm*il})
         else
              itot[j,2]+=pm*il
         endif
#endif
      endif
#ifdef A_MM
      @ _fk+1, NIMBEG-3 SAY nr_mag COLOR _sunsel
#endif
#ifdef A_ANKER
      @ _fk+1, NIMBEG SAY ean13(INDEX)
#endif
      
      @ _fk+1, NIMBEG SAY INDEX PICTURE "@R "+ INDEXPIC COLOR _sunsel


      do case
        case dok_p_r="S"
           s:=stanprzeD(_fnowy,'',s)
#ifdef A_JMO
#define restouT(x,p) if(x%p=0,str(x/p,6),stuff(str(int(x/p)+x%p/1000,10,3),7,1,"r"))
      if miar_opcja
        @ _fk+1,24 say restouT(s+il,(lam)->przel) COLOR _sunsel
      else
#endif
        @ _fk+1,24 say s+il PICTURE ILPIC COLOR _sunsel
#ifdef A_JMO
      endif
#endif
        case dok_kon="*"
          @ _fk+1,NZBEG SAY nr_zlec picture NZPIC color _sunsel
        case ""#dok_kon
          if dok_zew#"W"
            @ _fk+1,NZBEG SAY nr_zlec picture NZPIC color _sunsel
          else
            @ _fk+1,NZBEG SAY nr_zlec picture NZPIC color _sunsel
          endif
      endcase

#ifdef A_JMO
      if miar_opcja
        @ _fk+1,36 say restouT(pm*il,(lam)->przel) COLOR _sunsel
      else
#undef restouT
#endif
#ifdef A_SB
        @ _fk+1,24 say pm*il PICTURE ILPIC COLOR _sunsel
#else
        @ _fk+1,36 say pm*il PICTURE ILPIC COLOR _sunsel
#endif
#ifdef A_JMO
      endif
#endif
#ifndef A_WA
      dok_w1:=dok_war
#endif
      SHOWWAR(_f,{},3)
return
*********************************************
proc dok5(_f,getlist)
local rcrd,j,a,b,c,d,comdm,ames:={},x,y,z,cx,px
#ifdef A_ODDO
      setkey(K_F8,NIL)
#endif
#ifdef A_SPECYF
      setkey(92,NIL)
#endif
      if _fkey=K_ESC .and. _fnowy
         il:=0
#ifdef A_WA
         wa:=0
  #ifdef A_FA
         wz:=0
  #endif
#endif
         showwar(_f,getlist,gil)
      endif
#ifdef A_WA
#ifdef A_FA
      if updated().and.!(_fnowy .and. il=0 .and. dok_p_r<>"S" .and. wa=0 .and. wz=0)
#else
      if updated().and.!(_fnowy .and. il=0 .and. dok_p_r<>"S" .and. wa=0)
#endif
#else
#ifdef A_FA
      if updated().and.!(_fnowy .and.(_fkey=K_ESC .or. il=0 .and. dok_p_r<>"S".and. wz=0))
#else
      if updated().and.!(_fnowy .and.(_fkey=K_ESC .or. il=0 .and. dok_p_r<>"S"))
#endif
#endif
        CHANGED:=.T.
        LOCK IN DM
        if comdm:=_fnowy
//          _fnowy:=.f.
          append blank
          smb_dow:=dm->smb_dow
          nr_dowodu:=dm->nr_dowodu
          pozycja:=D_LPPUT(_fi)
          data:=dm->data
        ELSE
          lock
        endif
        IF _fi>posproc
           posproc:=_fi
        ENDIF
        IF pozycja>DM->pozycja
          DM->POZYCJA:=pozycja
          comdm:=.t.
        ENDIF
        nr_mag:=mag_biez
        index:=nim

#ifndef STANY
        INDX_MAT->(dbseek(MAIN->index,.f.))
#else
        INDX_MAT->(dbseek(MAIN->nr_mag+MAIN->index,.f.))
#endif        
        
        lam:=i_lam(da)
       
#ifdef A_GOCZ
        MAIN->dzial:=indx_mat->INFO
#endif
        if dok_kon="&:"
           nz:=&(trim(subs(dok_kon,3)))
           @ _fk+1,NZBEG say nz PICTURE NZPIC color _sbkgr
        endif
        nz:=nr_zlec:=UpP(nz)
#ifdef A_KPR
    if ""#dok_kpr .and. npr>DatY->last_kpr
       lock in daty
       DatY->last_kpr:=npr
#ifdef A_DDBF
       unlock in daty
#else
       inisave("daty.ini")
#endif
    endif
#endif
#ifdef A_FA
        if dok_p_r="F"
          replace dm->wartosc with dm->wartosC+(WGR(pm*il,cz,val(pv),dok_df)-WGR(pm*ilosc_f,cena,val(proc_vat),dok_df))/100
          if dok_zew$"UV"
             x:=WzVATGR(pm*ilosc_f,cena,val(proc_vat),dok_df)
             y:=ILEVATGR(pm*ilosc_f,cena,val(proc_vat),dok_df)
             vat(proc_vat,-y,y-x)
             x:=WzVATGR(pm*il,cz,val(pv),dok_df)
             y:=ILEVATGR(pm*il,cz,val(pv),dok_df)
             vat(pv,y,x-y)
             putVAT()
             proc_vat:=pv
#ifdef A_NVAT
             chgpos:=.t.
#endif
          endif
          cena:=cz
          comdm:=.t.
        elseif dok_ew#"E"
          replace dm->wartosc with dm->wartosC+(wz-pm*cena)
          if dok_zew$"UV"
             vat(proc_vat,-VATPZGR(pm*cena,val(proc_vat)),-pm*100*cena)
             vat(pv,VATPZGR(wz,val(pv)),100*wz)
             putVAT()
             proc_vat:=pv
          endif
          cena:=pm*wz
          comdm:=.t.
        endif
#endif
#ifdef A_SPECYF
         a:=specyfik(_f,1)
         if round(a-pm*il,3)=0
            specyfik(_f,2) //zapisz
         else
            specyfik(_f,-1) //kasuj ksp
         endif
#endif
#ifdef A_GRAM
#define D_D a:=
#else
#ifdef A_JMTOT
#define D_D a:=
#endif
#endif
#ifdef A_IZ
      if dok_ew#"E"
#ifdef D_D
         a:=il-ilosc_f
#endif
         ilosc_f:=il
      endif
      if dok_ew#"Z"
#endif
#ifndef A_WA
        wtoT-=ilosc*(lam)->cenA
#endif

        SELECT STANY

#ifndef STANY
        if NOWYSTAN
          APPEND BLANK
          INDEX:=main->INDEX
          NR_MAG:=main->NR_MAG
        ELSE
          LOCK
        ENDIF
        LOCK IN INDX_MAT
#else
        LOCK
#endif

#ifdef A_WA
     if main->ilosc#il .or. main->wartosc#wa
#else
     if il#main->ilosc
#endif
#ifdef A_FIFO
        IF dok_p_r<>'P' .and. il<0 .and. !chg_cen
           MAIN->(getck(.f.))
           ce:=round(ck,A_ZAOKR)
           wa:=round(il*ck,A_ZAOKR)
        ENDIF
        if fieldpos('stanx')<>0 .and. FIELD->VALIDX .and. da>=FIELD->DATAX
           if ROUND(FIELD->STANX+il-MAIN->ilosc,3)<0 .or. ROUND(il-MAIN->ilosc,3)>0 .or. ROUND(FIELD->STANX+il-MAIN->ILOSC-FIELD->MAXX,3)>0
              FIELD->VALIDX:=.f.
           else
              FIELD->STANX+=il-MAIN->ILOSC
              FIELD->WARTOSCX+=wa-MAIN->WARTOSC
           endif
        endif
#endif
#ifndef D_D
#define D_D
#endif
        STAN:=ROUND(STAN + ( D_D il-main->ILOSC ) ,3)
#ifdef A_ZAGRODA
#ifndef STANY
        IF INDX_MAT->zapas_id<>0
           STANY->unlink:=.t.
        ENDIF
#endif
	IF INDX_MAT->zaznacz<>2 .and. INDX_MAT->zapas_min>0 .and. STANY->stan<INDX_MAT->zapas_min
	   INDX_MAT->zaznacz:=2
	ELSEIF INDX_MAT->zaznacz=2 .and. INDX_MAT->zapas_min>0 .and. STANY->stan>=INDX_MAT->zapas_min
	   INDX_MAT->zaznacz:=0
	ENDIF
#endif
#undef D_D
        main->ilosc:=round(il,3)
#ifdef A_GRAM
        gtot+=pm*a*(lam)->gram/1000
#endif
#ifdef A_JMTOT
#ifdef A_JMALTTOT
           j:=ascan(itot,{|x|x[1]=(lam)->jm_opcja})
           if j=0
              aadd(itot,{(lam)->jm_opcja,A_JMALTTOT(pm*a,nz,lam,x)})
           else
              itot[j,2]+=A_JMALTTOT(pm*a,nz,lam,x)
           endif
#endif
           j:=ascan(itot,{|x|x[1]=(lam)->D_JM})
           if j=0
              aadd(itot,{(lam)->D_JM,a*pm})
           else
              itot[j,2]+=a*pm
           endif
#endif
#ifdef A_WA
#ifndef wtoT
        comdm:=.t.
#endif
        WARTOSC:=round(wartosc+ (a:=wa-main->WARTOSC),A_ZAOKR)
        wtoT+=a
        main->wartosc:=Round(wa, A_ZAOKR)
#ifdef A_CK
        MAIN->cena_k:=ck
#endif
#endif
#ifndef A_MINUS
#ifndef A_CENFIX
#ifdef A_FIFO
#ifdef A_SB
      if dok$dok_di
        a:=stanprzeD(.t.,'',a)
        if Round(a,ILDEC)<0
              aadd(ames,message('STAN TEGO DNIA :'+strpic(a,11,ILDEC,,.t.)+" "+indx_mat->jm+";"+INDX_MAT->NAZWA+';'+INDEX))
        endif
      endif
#endif
#else
        if pm=1 .and. main->data<=data_roz .or. pm=-1 .and. main->data<data_przy
          select main
          rcrd:=recno()
          set order to tag MAIN_IND
#ifdef A_WA
          sum ilosc,wartosc to a,b rest while nr_mag+index=STANY->(nr_mag+index)
#else
          sum ilosc to a rest while nr_mag+index=STANY->(nr_mag+index)
#endif
          go rcrd
          set order to tag MAIN_NRK
          select STANY
#ifdef A_WA
          c:=if(stan=a,0,(wartosc-b)/(stan-a)) // przed zaksięgowaniem
          d:=if(stan+main->ilosc=a,0,(main->wartosc+wartosc-b)/(main->ilosc+stan-a)) // po zaksięgowaniu
          if ROUND(b-c*a,A_ZAOKR)#0
             aadd(ames,message("Wsadzasz ten dokument w środek kartoteki !;Przed zaksięgowaniem cena na dzień "+dtoc(main->data)+";wynosiła "+ltrim(strpic(c,12,A_ZAOKR,"@E "))+", a po "+ltrim(strpic(d,12,A_ZAOKR,"@E "))+" zł."))
          endif
          b:=main->wartosc+wartosc-b
#endif
          a:=main->ilosc+stan-a
          IF a<0 .and. stan>=0
              aadd(ames,message('STAN UJEMNY TEGO DNIA :'+strpic(a,11,ILDEC,,.t.)+" "+indx_mat->jm+";"+INDX_MAT->NAZWA+';'+INDEX))
#ifdef A_WA
          ELSEIF b<0 .and. wartosc>=0
              aadd(ames,message('WARTOŚĆ UJEMNA TEGO DNIA :'+strpic(b,12,A_ZAOKR,"@E ")+";"+INDX_MAT->NAZWA+';'+INDEX))
          ELSEIF b#0 .AND. a=0  .and. (wartosc=0 .or. stan#0)
              aadd(ames,message('WARTOŚĆ BEZ STANU TEGO DNIA :'+strpic(b,12,A_ZAOKR,"@E ")+";"+INDX_MAT->NAZWA+';'+INDEX))
#endif
#ifndef A_NOZAP
          ELSEIF INDX_MAT->zapas_min # 0 .and. a<INDX_MAT->zapas_min .and. stan>=indx_mat->zapas_min
              aadd(ames,message('ZA MAŁY ZAPAS TEGO DNIA :'+strpic(a,11,ILDEC,,.t.)+" "+INDX_MAT->JM+";"+INDX_MAT->NAZWA+';'+INDEX))
          ELSEIF INDX_MAT->zapas_max # 0 .and. a>INDX_MAT->zapas_max .and. stan<=indx_mat->zapas_max
              aadd(ames,message('ZA DUŻY ZAPAS TEGO DNIA :'+strpic(a,11,ILDEC,,.t.)+" "+INDX_MAT->JM+";"+INDX_MAT->NAZWA+';'+INDEX))
#endif
          ENDIF
        endif
#endif
#endif
        IF STAN<0
              aadd(ames,message('STAN UJEMNY :'+str(STAN)+" "+indx_mat->jm+";"+INDX_MAT->NAZWA+';'+INDEX))
#ifdef A_WA
        ELSEIF WARTOSC<0
              aadd(ames,message('WARTOŚĆ UJEMNA :'+strpic(WARTOSC,,,"@E")+";"+INDX_MAT->NAZWA+';'+INDEX))
        ELSEIF WARTOSC#0 .AND. STAN=0
              aadd(ames,message('WARTOŚĆ BEZ STANU :'+strpic(WARTOSC,,,"@E")+";"+INDX_MAT->NAZWA+';'+INDEX))
#endif
#ifndef A_NOZAP
        ELSEIF INDX_MAT->zapas_min # 0 .and. stan<INDX_MAT->zapas_min
              aadd(ames,message('ZA MAŁY ZAPAS :'+str(STAN)+" "+INDX_MAT->JM+";"+INDX_MAT->NAZWA+';'+INDEX))
        ELSEIF INDX_MAT->zapas_max # 0 .and. stan>INDX_MAT->zapas_max
              aadd(ames,message('ZA DUŻY ZAPAS :'+str(STAN)+" "+INDX_MAT->JM+";"+INDX_MAT->NAZWA+';'+INDEX))
#endif
        ENDIF
#endif
     endif
#ifdef A_WA
     if il#0 .or. wa#0
#else
     if il#0
#endif

        if main->data>data_zmian
          DATA_ZMIAN:=main->DATA
        endif
        if main->data>data_roz .and. main->ilosc<0 .and. pm=-1
          DATA_roz:=main->DATA
        endif
        IF main->data>=data_przy .and. pm=1 .and. main->ilosc>=0 .and. dok#'K'
#ifdef A_AUTOMAR
#ifndef cenA_zaK
          if dok_ew#"E"
#endif
/*
#ifndef STANY
          lam:=i_lam(da)
#endif
*/
          b:=indx_mat->proc_mar

#ifdef cenA_zaK
          cz:=ce
#endif

          px:=INDX_MAT->proc_vat
#ifdef A_NOPV
#else
          if empty(px)
            px:=pv
          endif
#endif
          cx:=eval(MEMVAR->liczCzM,cz,val(px),b,indx_mat->przel)
            IF INDX_MAT->cena<>0 .and.( (ROUND(cenA_zaK-cz,A_ZAOKR)<>0 .and. ROUND(INDX_MAT->cena-cx,A_ZAOKR)<>0) .OR. INDX_MAT->PROC_VAT#px )
              if lam=SELECT("INDX_MAT")
/*************
             commit in main
             commit in STANY
                  if comdm
             commit in dm
             comdm:=.f.
                  endif
***************/
                IF cx#0 .and. cz#0 .and. il#0
            a:=message("Stara cena: "+strpic(indx_mat->cena,12,A_ZAOKR,"@E ")+"; Nowa cena:;               Marża:    %;   [Esc] - rezygnacja")
            c:={getnew(a[1]+2,a[2]+14,{|x|if(pcount()=0,cx,cx:=x)},"cx",WAPIC),getnew(a[3]-2,a[4]-5,{|x|if(pcount()=0,b,b:=x)},"b","##")}
            c[1]:postblock:={|g|!g:changed .or.(b:=eval(memvar->liczm,cz,cx,val(px)),c[2]:display(),.t.)}
            c[2]:postblock:={|g|!g:changed .or.(cx:=eval(memvar->liczczm,cz,val(px),b,indx_mat->przel),c[1]:display(),.t.)}
            c[2]:display()
            d:=1
                  Do While .t.
              readmodal(c,d)
              exit
                  enddo
            message(a)
                  if readkey()#K_ESC
                    if da>indx_mat->data_popr
                SELECT IND_LAM
                APPEND BLANK
                     for j=1 to fcount()
                  fieldput(j,indx_mat->(fieldget(j)))
                     next
                INDX_MAT->lamus:=recno()
                select STANY
                indx_mat->data_popr:=da
                   endif
#ifdef A_ANKER
                   if isdigit(indx_mat->index) .and. (ROUND(indx_mat->cena-cx,A_ZAOKR)<>0 .or. indx_mat->proc_vat#px)
                indx_mat->status:=4*int(indx_mat->status/4)+2
                indx_mat->zaznacz:=1
                   endif
#endif
              indx_mat->PROC_VAT:=px
#ifdef A_ALTCEN
 #ifdef A_ANKER
  #ifdef A_ZAGRODA
            indx_mat->proc_mar1:=max(10,b/5)
            indx_mat->cena1:=eval(MEMVAR->liczCzM,cz,val(px),indx_mat->proc_mar1,indx_mat->przel)
          //indx_mat->cena2:=eval(MEMVAR->liczCzM,cz,val(px),indx_mat->proc_mar2,indx_mat->przel)
            indx_mat->proc_mar2:=eval(memvar->liczM,indx_mat->cena2,cz,val(px))
            indx_mat->cena3:=eval(MEMVAR->liczCzM,cx,0,indx_mat->proc_mar3,indx_mat->przel)
  #else
            indx_mat->cena1:=eval(MEMVAR->liczCzM,cx,0,indx_mat->proc_mar1,indx_mat->przel)
            indx_mat->cena2:=eval(MEMVAR->liczCzM,cx,0,indx_mat->proc_mar2,indx_mat->przel)
            //indx_mat->cena3:=eval(MEMVAR->liczCzM,cx,0,indx_mat->proc_mar3,indx_mat->przel)
  #endif
 #else
            indx_mat->cena1:=eval(MEMVAR->liczCzM,cx,val(px),indx_mat->proc_mar1,indx_mat->przel)
            indx_mat->cena2:=eval(MEMVAR->liczCzM,indx_mat->cena1,val(px),indx_mat->proc_mar2,indx_mat->przel)
            indx_mat->cena3:=eval(MEMVAR->liczCzM,indx_mat->cena1,val(px),indx_mat->proc_mar3,indx_mat->przel)
 #endif
#endif
            indx_mat->cena:=cx
            indx_mat->proc_mar:=b

                  endif

                ENDIF
              else
            aadd(ames,message("NIE ZMIENIŁEM CENY ZBYTU, PONIEWAŻ DATA POPRAWY KARTY MATERIAŁU JEST PÓŹNIEJSZA OD DATY WYSTAWIENIA TEGO DOKUMENTU"))
              endif
            endif
#ifndef cenA_zaK
          endif
#endif
#else
#ifdef A_FA
#ifndef A_NOPV
          if !empty(pv) .and. empty(indx_mat->proc_vat) //<>pv
            indx_mat->proc_vat:=pv
          endif
#endif
#endif
#endif //AUTOMAR
          DATA_PRZY:=main->DATA
#ifdef A_WA
          CENA_PRZY:=ce
#endif
#ifdef A_FA
#ifndef cenA_zaK
          cenA_zaK:=cz
#endif
#endif

        endif
        endif
        select main
#ifdef A_IZ
else
#ifdef A_GRAM
        gtot+=pm*a*(lam)->gram/1000
#endif
#ifdef A_JMTOT
#ifdef A_JMALTTOT
           j:=ascan(itot,{|x|x[1]=(lam)->jm_opcja})
           if j=0
              aadd(itot,{(lam)->jm_opcja,A_JMALTTOT(pm*a,nz,lam,x)})
           else
              itot[j,2]+=A_JMALTTOT(pm*a,nz,lam,x)
           endif
#endif
           j:=ascan(itot,{|x|x[1]=(lam)->D_JM})
           if j=0
              aadd(itot,{(lam)->D_JM,a*pm})
           else
              itot[j,2]+=a*pm
           endif
#endif
      endif
#endif
#ifdef A_WA
#ifdef A_FA
        if il=0 .and. wa=0 .and. wz=0 .and. ! ( dok_p_r="S" .and. _fnowy )
#else
        if il=0 .and. wa=0 .and. ! ( dok_p_r="S" .and. _fnowy )
#endif
#else
        wtoT+=ilosc*(lam)->cenA
        showwar(_f,getlist,gil)
#ifdef A_FA
        if il=0 .and. wz=0 .and. ! ( dok_p_r="S" .and. _fnowy )
#else
        if il=0 .and. ! ( dok_p_r="S" .and. _fnowy )
#endif
#endif
          --posproc
          delete
          comDM:=_fnowy:=.t.
          DM->POZYCJA:=D_LPPUT(_flp-1)
         if _fi<_flp
          a:=recno()
          skip -1  // po zmianie pozycji byłby niepewny układ, w dm_form i tak skip
          IF !BOF()
             a:=recno()
          ELSE
             GO a
          ENDIF
          b:=_fi
          for b=_fi to _flp-1
             skip
#ifdef A_LAN
             IF reclock()
#endif
#ifdef A_SPECYF
                specyfik(_f,3,pozycja,D_LPPUT(b))
#endif
                pozycja:=D_LPPUT(b)
#ifdef A_LAN
             ENDIF
#endif
          next
          go a
         endif
	else
	 _fnowy:=.f.
        endif
#ifdef A_KPR
    do while ""#dok_kpr
       if npr>DatY->last_kpr .and. dm->pozycja>D_LP0
         a:=npr
       elseif npr=DatY->last_kpr .and. dm->pozycja=D_LP0
         a:=npr-1
       else
         Exit
       endif
       lock in daty
       DatY->last_kpr:=a
#ifdef A_DDBF
       unlock in daty
#else
       inisave("daty.ini")
#endif
       Exit
    enddo
#endif
        if comdm
          commit in DM
        endif
      elseif dok_kon="&:"
         x:=trim(subs(dok_kon,3))
         (&x,x:=NIL)
      ENDIF

      UNLOCK
      UNLOCK IN STANY

#ifndef STANY
      UNLOCK IN indx_mat
#endif
      if (a:=len(ames))>0
         a:=ames[a]
         @ a[3],a[2] SAY left(hb_UTF8ToStr("Naciśnij coś..."),a[4]-a[2])
         tone(130,3)
         inkey(0)
         for a:=len(ames) to 1 step -1
             message(ames[a])
         next
      endif
return
*******************************
procedure dok6(_f)
  select DM
  IF dok_kop<0 .or. dok_kop>0 .and. TAK(hb_UTF8ToStr('CZY DRUKOWAĆ (DANE SĄ POPRAWNE ?)'),ROW(),25,.T.,.F.)
    wydruk_dok(abs(dok_kop))
//#ifdef A_OBR
  else //if dok_kop<=0 .and. TAK(hb_UTF8ToStr('CZY DANE SĄ POPRAWNE'),ROW(),25,.T.,.F.)
    KTO_PISAL:=OPERATOR
    changed:=.t.
//#endif
  ENDIF
return 
*****************
procedure dok7(_f)
local job,bx,cx,dx
memvar operator
set cursor off

do while _fpopkey

//@ _fk,_fco1 SAY '>' COLOR _sbkgr
//@ _fk+_fskip-1,_fco2 SAY '<' COLOR _sbkgr


#ifdef __HARBOUR__
#define D_REST 4
#else
#define D_REST 2
#endif
#ifdef A_LAN
dm->(dbgoto(recno()))
if _flp>D_LPVAL(dm->pozycja) .or. if(_flp=0, dm->pozycja >D_LP0,_fi#D_LPVAL(pozycja)) .and. D_LPVAL(pozycja)>0
   select dm
   _fj:=0
   _fl:=_fi:=1
   RESTSCREEN(_frow+2,_fco1,maxrow(),_fco2,SUBSTR(_fscr,D_REST*(_fco2-_fco1+1)*(_frow+2)+1))
   @ _frow+2,_fco1,_frow+4,_fco2 BOX UNICODE if(pozycja>D_LP1,'║ ║║╜─╙║ ','║ ║║╝═╚║ ') color _SBKGR
   _fpopkey:=.f.
   dok11(_f)
   _fkey:=K_ESC
   inkey()
   exit
elseif _flp<D_LPVAL(dm->pozycja)
   _flp:=D_LPVAL(dm->pozycja)
   _fkey:=K_PGDN
else
#ifdef A_MYSZ
#ifdef __HARBOUR__
     _fkey:=inkey(2, INKEY_KEYBOARD + INKEY_LDOWN + INKEY_RDOWN)
     bx:=cx:=dx:=0
     if _fkey>=K_MINMOUSE .and. _fkey<=K_MAXMOUSE
        if _fkey=1002
           bx:=1
        else
           bx:=2
        endif
        cx:=mcol()
        dx:=mrow()
        _fkey:=GE_MOUSE
     endif
#else
     sysint(51,1)
     job:=seconds()+2
     bx:=cx:=dx:=0
     do while (_fkey:=inkey())=0 .and. job>seconds()
        sysint(51,3,@bx,@cx,@dx)
        if bx#0
            cx:=int(cx/8+.1)
            dx:=int(dx/8+.1)
            _fkey:=GE_MOUSE
            exit
        endif
    enddo
    sysint(51,2)
#endif
#else
   _fkey:=inkey(2)
#endif
   if _fkey=0
     DatY->(dbgoto(1))
     if dm->kto_pisal#HB_UTF8CHR(0x00A0) .or. dm->data<=max(DatY->d_z_mies1,DatY->data_gran)
        loop //niepotrzebny refr
     elseif _fi=_fl
        skip _fj+1-_fi
        _fi:=_fj+1
     else
        ++_fi
        skip
     endif
     dm->(dbgoto(recno()))
     exit
   endif
endif
#else
#ifdef A_MYSZ
#ifdef __HARBOUR__
     _fkey:=inkey(0, INKEY_KEYBOARD + INKEY_LDOWN + INKEY_RDOWN)
     bx:=cx:=dx:=0
     if _fkey>=K_MINMOUSE .and. _fkey<=K_MAXMOUSE
        if _fkey=1002
           bx:=1
        else
           bx:=2
        endif
        cx:=mcol()
        dx:=mrow()
        _fkey:=GE_MOUSE
     endif
#else
     sysint(51,1)
     bx:=cx:=dx:=0
     do while (_fkey:=inkey())=0
        sysint(51,3,@bx,@cx,@dx)
        if bx#0
            _fkey:=GE_MOUSE
            cx:=int(cx/8+.1)
            dx:=int(dx/8+.1)
            exit
        endif
     enddo
     sysint(51,2)
#endif
#else
   _fkey:=inkey(0)
#endif
#endif

//@ _fk,_fco1 SAY '║' COLOR _sbkgr
//@ _fk+_fskip-1,_fco2 SAY '║' COLOR _sbkgr

#ifdef A_MYSZ
   if _fkey=GE_MOUSE
      readkey(_fkey,{bx,cx,dx})
      exit
   endif
#endif

  do case
    case _fkey=K_UP;_fkey:=K_PGUP
    case _fkey=K_DOWN;_fkey:=K_PGDN

#ifdef A_ZATW
    case _fkey=61 .and. canopen .and. iS_spec .and. dm->data>max(DatY->d_z_mies1,DatY->data_gran) .and. dm->kto_pisal#HB_UTF8CHR(0x00A0)
       if dm->kto_pisal#operator
         select DM
         LOCK
         kto_pisal:=operator
         UNLOCK
         changed:=.t.
       endif
       @ 0,0
       @ 0,0 say "ZATWIERDZONY !"
        tone(164.8,1)
        tone(164.8,1)
       kibord(chr(27))
       _fkey:=inkey()
       exit
#endif

#ifdef A_KONTROLA
    case (_fkey=K_CTRL_RET .or. _fkey=K_ALT_RETURN ).and. canopen .and. A_KONTROLA .and. alarm(hb_UTF8ToStr("CZY OTWORZYĆ DOKUMENT ")+dm->smb_dow+dm->nr_dowodu,{"Tak","Nie"})=1
#else
    case (_fkey=K_CTRL_RET .or. _fkey=K_ALT_RETURN ).and. canopen .and. iS_spec .and. alarm(hb_UTF8ToStr("CZY OTWORZYĆ DOKUMENT ")+dm->smb_dow+dm->nr_dowodu,{"Tak","Nie"})=1
       BEGIN SEQUENCE
       hAslo_spec(0)
       RECOVER
       select main
       LOOP
       END SEQUENCE
#endif
       select DM
       lock
       kto_pisal:=HB_UTF8CHR(0x00A0)+kto_pisal
       changed:=.t.
       UNLOCK
       @ 0,0
       @ 0,0 say "OTWARTY !"
        tone(164.8,1)
        tone(164.8,1)
#ifdef A_DF
       if dok_df .and. kto_pisal#HB_UTF8CHR(0x00A0)+chr(0)
          dispout(hb_UTF8ToStr(" - PONOWNY WYDRUK PARAGONU NIEMOŻLIWY !"))
          inkey(5)
       endif
#endif
       kibord(chr(K_ESC)+chr(K_ENTER))
       _fkey:=inkey()
#ifdef A_KONTROLA
       cx:=dtoc(date())+' '+time()+' '+operator+chr(K_ENTER)+chr(K_CTRL_RET)
       bx:={|x,i|if(x[2]$'CNDB'.and.x[1]#'D_',cx+=tran(fieldget(i),)+'|',)}
       aeval(dbstruct(),bx)
       cx+=chr(K_ENTER)+chr(K_CTRL_RET)
       SELECT MAIN
       SEEK DM->(KEY_DOK+NR_DOWODU)
       dx:=dbstruct()
#ifdef __PLATFORM__UNIX
#define nl chr(10)
#else
#define nl chr(13)+chr(10)
#endif
       exec {||aeval(dx,bx),cx+=nl} WHILE KEY_DOK+NR_DOWODU=DM->(KEY_DOK+NR_DOWODU)
       SELECT DM
       bx:=fopen(defa+'kontrola',33) //DENY WRITE
       if bx=-1
          bx:=fcreate(defa+'kontrola',2)
       endif
       fseek(bx,0,2)
       fwrite(bx,cx)
       fclose(bx)
#endif
       exit
  endcase

  do case
    case _fkey=K_PGUP .and. _fj>0
      skip _fj+1-_fi
      _fi:=_fj+1
    case _fkey=K_PGDN .and. _fl<_flp
      skip _fl-_fi
      _fi:=_fl
    case _fkey#K_CTRL_L .and. _fkey#K_ESC
      loop
  endcase
  exit
enddo

return
*****************
#ifdef A_WA
procedure add_get(_f,getlist)
  local get
  if ascan(getlist,{|x|x:name="ce"})=0
  @ _fk+1,53 get ce picture WAPIC  valid  {|g|!g:changed.or.gcen(_f,getlist)}
  get:=getnew(_fk+1,66,{|x|if(pcount()=0,ROUND(pm*wa,A_ZAOKR),wa:=pm*ROUND(x,A_ZAOKR))},"wa",WAPIC)
  get:postblock:={|g|!g:changed.or.gwar(_f,getlist)}
  get:display()
  aadd(getlist,get)
  endif
return
#endif
***************
function showwar(_f,getlist,gil)
local j,clr:=_sbkgr
   @ _fk,52 CLEAR TO _fk+1,_fco2-2
#ifdef A_GRAM
   @ (_fl-_fj+1)*_fskip+_frow,1 SAY str(gtot+if(lam=NIL,0,pm*if(_fnowy,il,il-main->ilosc)*(lam)->gram/1000),8,2)+" kg" color _sbkgr
#endif
#ifdef A_JMTOT
#ifdef A_GOCZ
if !empty(getlist) .and. dok$dok_di
   getlist[gil-1]:display()
endif
#else
#ifdef A_GRAM
   setpos(row(),15)
#else
   setpos((_fl-_fj+1)*_fskip+_frow,1)
#endif
for j=1 to len(itot)
   if itot[j,2]=0
      loop
   endif
#ifdef A_IZ
   devout(ltrim(str(itot[j,2]+if(lam#NIL.and.itot[j,1]=(lam)->jm,pm*if(_fnowy,il,il-if(dok_ew="E",main->ilosc,main->ilosc_f)),0),16,3))+" "+trim(itot[j,1]),_sbkgr)
#else
   devout(ltrim(str(itot[j,2]+if(lam#NIL.and.itot[j,1]=(lam)->jm,pm*if(_fnowy,il,il-main->ilosc),0),16,3))+" "+trim(itot[j,1]),_sbkgr)
#endif
   setpos(row(),col()+1)
next
#endif
#endif
#ifdef A_FA
      if dok_p_r="F"
         showfak(_f,if(dok_war#"-".or.len(getlist)>gil,_sunsel,_sbkgr))
      elseif dok_ew#"E"
         showvat(_f)
         if dok_p_r="P" .and. dok_war="-"
            return .t.
         endif
#ifdef cenA_zaK
#ifdef A_WA
         @ (_fl-_fj+1)*_fskip+_frow,66 say pm*(wtoT+wa-if(_fnowy,0,main->wartosc)) PICTURE WAPIC COLOR _Sbkgr
#else
         @ (_fl-_fj+1)*_fskip+_frow,66 say pm*(wtoT+if(lam=NIL,0,(il-if(_fnowy,0,main->ilosc))*(lam)->cenA)) PICTURE WAPIC COLOR _Sbkgr
#endif
         return .t.
#endif
      endif
#endif
#ifdef A_IZ
    if dok_ew="Z"
       return .t.
    endif
#endif
#ifdef A_WA
if !empty(getlist)
@ _fk+1,52 say if(chg_cen,"■","✓") color _sbkgr
endif
if dok_war="+".or.(!empty(getlist).and.ascan(getlist,{|x|x:name="wa"})#0)
   clr:=_sunsel
elseif dok_war="-"
   return .t.
endif
@ (_fl-_fj+1)*_fskip+_frow,66 say pm*(wtoT+wa-if(_fnowy,0,main->wartosc)) PICTURE WAPIC COLOR _Sbkgr
#ifdef A_AUTOMAR
#ifdef cenA_zaK
if dok_ew#'E' .and. dok_p_r#'F'
   return .t.
endif
#else
if dok_ew#'E' .and. dok_p_r='P' .and. dok_war#'+' .and. clr#_sunsel .and. wa=wz
   return .t.
endif
#endif
 if dok_ew='E' .and. dok_p_r='P'
   @ _fk,53 say str(eval(MEMVAR->liczM,ce,(if(lam=NIL,select("INDX_MAT"),lam))->cena,val(pv)),4,1)+'%'
   sayL str((if(lam=NIL,select("INDX_MAT"),lam))->cena)
 endif
#endif
@ _fk+1,53 say ce picture WAPIC COLOR clr
@ _fk+1,66 say pm*wa PICTURE WAPIC COLOR clr
#else
if dok_w1#"-"
if lam#NIL
@ _fk+1,53 say (lam)->cenA picture WAPIC COLOR _sbkgr
@ _fk+1,66 say pm*il*(lam)->cenA PICTURE WAPIC COLOR _sbkgr
endif
@ (_fl-_fj+1)*_fskip+_frow,66 say pm*(wtoT+if(lam=NIL,0,(lam)->cenA*(il-if(_fnowy,0,main->ilosc)))) PICTURE WAPIC COLOR _Sbkgr
endif
#endif
return .t.
******************
#ifdef A_FA
func add_faget(_f,getlist)
   local get,x
#ifndef A_NOPV
   if dok_zew$"UV"
      @ _fk,49 get pv picture "@K XX" valid {||pv:=lower(pv),if(pv<="9",pv:=str(val(pv),2),),(ascan(stawkizby,pv)#0 .or. alarm('NIEWŁAŚCIWY PROCENT VAT!',,3,3)=NIL) .and. (wz:=W(pm*il,cz,val(pv),dok_df),showfak(_f,_sunsel))}
   endif
#endif
    get:=getnew(_fk,53,{|x|if(pcount()=0,cz,cz:=x)},"cz",WAPIC)
#ifdef A_KOB
    get:postblock:={|g|if(""=dok_war.and.ce=0,(chg_cen:=.t.,ce:=cz,wa:=pm*(wz:=W(pm*il,cz,val(pv),dok_df)),showwar(_f,getlist,gil)),!g:changed.or.(wz:=W(pm*il,cz,val(pv),dok_df),showfak(_f,_sunsel)))}
#else
    get:postblock:={|g|!g:changed.or.(wz:=W(pm*il,cz,val(pv),dok_df),showfak(_f,_sunsel))}
#endif
#ifdef A_IZ
    if dok_ew#"Z"
#endif
#ifdef A_WA
    get:reader:={|g|setkey(61,{|p|add_get(_f,getlist),setkey(61,NIL)}),getreader(g),setkey(61,NIL)}
#else
    if dok_war="-"
        get:reader:={|g|setkey(61,{||dok_w1:="+",showwar(_f,getlist,gil),setkey(61,NIL)}),getreader(g),setkey(61,NIL)}
    endif
#endif
#ifdef A_IZ
    endif
#endif
    get:display()
    aadd(getlist,get)
return .t.
*****************
func showfak(_f,clr)
   local wt:=dm->wartosc,vt:=dm->wart_vat,x
   if !przegl
#ifdef wartosC
      wt:=dm->wartosC
#endif
      if !_fnowy
         wt-=WGR(pm*ilosc_f,cena,val(proc_vat),dok_df)/100
      endif
      wt+=WGR(pm*il,cz,val(pv),dok_df)/100
   if dok_zew$"UV"
#ifdef A_NVAT
      vt:=avat
      private avat:=aclone(vt)
      if !_fnowy
         vat(proc_vat,-ILEVATGR(pm*ilosc_f,cena,val(proc_vat),dok_df))
      endif
      vat(pv,ILEVATGR(pm*il,cz,val(pv),dok_df))
      vt:=vat()
#ifdef A_DF
#ifdef A_CENVAT
      if !dok_df
         wt:=ROUND(wt-vat(,),A_ZAOKR)+vt
#else
      if dok_df
         wt:=ROUND(wt+vat(,),A_ZAOKR)-vt
#endif
      endif
#endif

#else
      if !_fnowy
         vt-=ILEVATGR(pm*ilosc_f,cena,val(proc_vat),dok_df)/100
      endif
      vt+=ILEVATGR(pm*il,cz,val(pv),dok_df)/100
      vt:=round(vt,A_ZAOKR)
#endif
   endif
      wt:=round(wt,A_ZAOKR)
   endif
   @ _fk,53 say cz picture WAPIC COLOR clr
   @ _fk,66 say wz PICTURE WAPIC COLOR _sbkgr
   if dok_zew$"UV"
      @ _fk,49 say pv COLOR clr
   endif
   showfakh(_f,wt,vt)
return .t.
*****************
func showfakh(_f,wt,vt)
IF _frow<7
   @ _frow,14 say ZAG(wt,vt) PICTURE WAPIC COLOR _Sbkgr
ELSE
   @ 4,26 say wt PICTURE WAPIC COLOR _Sbkgr
   @ 6,4 say ZAG(wt,vt) PICTURE WAPIC COLOR _Sbkgr
   if dok_zew$"UV"
      @ 4,39 say vt PICTURE "@E #######.##" COLOR _Sbkgr
   endif
#ifdef A_FK
   if dok_fk
      @ _frow-1,54 say WBR(wt,vt)-zaplac PICTURE WAPIC COLOR _Sbkgr
   endif
#endif
ENDIF
return .t.
*************
func showvat(_f)
   local wt:=dm->wartosc,vt:=dm->wart_vat
   if !przegl
#ifdef wartosC
      wt:=dm->wartosC
#endif
      if !_fnowy
         wt-=pm*cena
      endif
      wt+=wz
   if dok_zew$"UV"
      vt:=avat
      private avat:=aclone(vt)
      if !_fnowy
         vat(proc_vat,-VATPZGR(pm*cena,val(proc_vat)))
      endif
      vat(pv,VATPZGR(wz,val(pv)))
      vt:=vat()
   endif
      wt:=round(wt,A_ZAOKR)
   endif
   @ _fk,53 say cz picture WAPIC COLOR _sunsel
   @ _fk,52 say if(wz=WPZ(pm*il*cz),"✓","■") color _sbkgr
   @ _fk,66 say wz picture WAPIC COLOR _sunsel
#ifdef A_AUTOMAR
//#ifdef cenA_zaK
 if dok_war#'+' .and. dok_p_r='P'
   @ _fk+1,53 say str(eval(MEMVAR->liczM,cz,(if(lam=NIL,select("INDX_MAT"),lam))->cena,val(pv)),4,1)+'%'
   sayL str((if(lam=NIL,select("INDX_MAT"),lam))->cena)
 endif
//#endif
#endif
   if dok_zew$"UV"
#ifdef A_NOPV
      @ _fk,49 say pv COLOR _sbkgr
#else
      @ _fk,49 say pv COLOR _sunsel
#endif
   endif
#ifndef A_WEBRUT
#ifdef cenA_zaK
   if dok_war#'-' .and. dok_p_r='P' .and. dok_zew$"UV"
#else
   if dok_war#'-' .and. dok_p_r='P'  .and. dok_zew$"UV" .and. dok_ew='Z'
#endif
      //if dok_df
        @ _fk+1,53 say ROUND((wz+VATPZ(wz,val(pv)))/il,A_ZAOKR) picture WAPIC COLOR _Sbnorm
        @ _fk+1,66 say wz+VATPZ(wz,val(pv)) picture WAPIC COLOR _Sbnorm
      //endif
      @ (_fl-_fj+1)*_fskip+_frow,66 say wt PICTURE WAPIC COLOR _Sbkgr
   endif
#endif
   showvath(_f,wt,vt)
return .t.
*************
func showvath(_f,wt,vt)
#ifdef A_PZBRUT
   @ _frow,26 say wt PICTURE WAPIC COLOR _Sbkgr
#else
   @ _frow,26 say wt+vt PICTURE WAPIC COLOR _Sbkgr
#ifndef A_DATAVAT
   if dok_p_r="P" .and. dok_ew#"Z"
     @ _frow,48 say wtoT-wt PICTURE STRTRAN(WAPIC,"E","EZ") COLOR _Sbkgr
   endif
#endif
#endif
#ifdef A_WEBRUT
   if dok_p_r="P"
     @ _frow,48 say wt PICTURE STRTRAN(WAPIC,"E","EZ") COLOR _Sbkgr
   endif
#endif
#ifdef A_FK
  if dok_fk
     @ 3,66 say wt+vt-zaplac PICTURE WAPIC COLOR _Sbkgr
  endif
#endif
  if dok_zew$"UV"
     @ _frow,39 say vt PICTURE "@E #######.##" COLOR _sunsel
  endif
return .t.
***********
function vat(pv,v,n,p)
local k
   if empty(pv)
      v:=0;n:=0;p:=0
      if pcount()>1
         aeval(avat,{|x|v+=x[2],n+=x[3],p+=x[4]})
      else
         aeval(avat,{|x|v+=ROUND(x[2],0),n+=ROUND(x[3],0),p+=ROUND(x[4],0)})
      endif
      v/=100;n/=100;p/=100
   else
      k:=ascan(avat,{|x|x[1]=pv})
      if v#NIL
         if n=NIL
            n:=0
         endif
         if p=NIL
            p:=0
         endif
         if k=0
            aadd(avat,{pv,v,n,p})
         else
            v:=avat[k,2]+=v;n:=avat[k,3]+=n;p:=avat[k,4]+=p
         endif
      elseif k#0
         v:=avat[k,2];n:=avat[k,3];p:=avat[k,4]
      else
         n:=v:=p:=0
      endif
   endif
return v
*************
proc putvat
local x,i,n,v,l,y
#ifdef A_NVAT
#define D_NVAT "D_VAT"
#else
#define D_NVAT "WART_VAT"
  #define bin2D(x) (100*(x))
  #define field2biN(x,y) fieldput(x,(y)/100)
//#define d2biN(x) ((x)/100)
#endif

dm->wart_vat:=vat()
l:=len(stawki)
for i:=1 to l
    x:=stawki[i]
    v:=NIL
    vat(x,@v,@n)
    if val(x)#0
       y:=dm->(fieldpos(D_NVAT+ltrim(x)))
       dm->(field2biN(y,v))
#ifdef A_A
 #ifdef A_NVAT
       y:=dm->(fieldpos('wart_vat'+ltrim(x)))
       dm->(fieldput(y,round(v,0)/100))
 #endif
    endif
 #undef D_CENVAT
 #ifdef A_NVAT
  #ifdef A_DF
    #define D_CENVAT dok_p_r="F".and.dok_df
  #else
   #ifdef A_CENVAT
    #define D_CENVAT dok_p_r="F"
   #endif
  #endif
 #endif
 #ifdef D_CENVAT
    if D_CENVAT
       y:=dm->(fieldpos('wart_net'+ltrim(x)))
       dm->(fieldput(y,(round(n+v,0)-round(v,0))/100))
    else
 #endif
       y:=dm->(fieldpos('wart_net'+ltrim(x)))
       dm->(fieldput(y,n/100))
 #ifdef D_CENVAT
    endif
 #endif
#else
    endif
#endif
next i
#ifdef A_DF
#ifdef A_CENVAT
if dok_p_r="F" .and. !dok_df
   x:=vat(,)
   dm->wartosc:=bin2d(DM->(binfieldget([d_wartosc])))-x+dm->wart_vat
   field2bin([d_wartosc],dm->wartosc-dm->wart_vat+x,1)
#else
if dok_df
   x:=vat(,)
   dm->wartosc:=bin2d(DM->(binfieldget([d_wartosc])))+x-dm->wart_vat
   field2bin([d_wartosc],dm->wartosc+dm->wart_vat-x,1)
#endif
endif
#endif
return
***********
proc getvat()
local i,l,x,n,v,y
avat:={}
l:=len(stawki)
for i:=1 to l
    x:=stawki[i]
    v:=0
#ifdef A_A
    y:=dm->(fieldpos('wart_net'+ltrim(x)))
    n:=dm->(fieldget(y))
    if !empty(n)
       n*=100
    endif
    //n:=dm->&("WART_NET"+ltrim(x))*100
#endif
    if val(x)#0
       y:=dm->(fieldpos(D_NVAT+ltrim(x)))
       IF y<>0
          v:=bin2D(dm->(binfieldget(y)))
       ENDIF
    endif
    vat(x,v,n)
next i
return
#endif
***************
