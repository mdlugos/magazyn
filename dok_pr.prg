#include "dm_form.ch"
#include "inkey.ch"

#ifdef A_LAN
#ifndef A_DDBF
  #define A_DDBF
#endif
#endif

#ifndef A_DDBF
   #define DatY MEMVAR
#endif

#ifdef A_FK
  #ifndef A_KHNUM
  #define A_KHNUM
  #endif
#endif

#ifdef A_KHNAZ
  #ifndef A_DMDATA
  #define A_DMDATA
  #endif
  #ifndef A_KHNUM
  #define A_KHNUM
  #endif
#endif

#ifdef A_F9
  #ifndef A_KHNUM
  #define A_KHNUM
  #endif
  #ifdef A_ANKER
  #ifndef A_KHNAZ
    #define A_KHNAZ 23
  #endif
  #endif
#endif

#define WAPICT "@E ### ###.##"

#ifdef A_SZTUKI
  #define ILDEC 0
#else
  #define ILDEC 3
#endif


#ifdef A_MM
  #define KEY_DOK smb_dow
  #define D_MM ''
#else
  #define KEY_DOK nr_mag+smb_dow
  #define D_MM nr_mag
#endif


#ifdef A_FA

#ifdef A_CENVAT
#define BRUTTO if(KEY_DOK$dok_fak,wartosc,wartosc+wart_vat)
#define NETTO if(KEY_DOK$dok_fak,wartosc-wart_vat,wartosc)
#else
#define BRUTTO wartosc+wart_vat
#define NETTO wartosc
#endif

#ifdef A_FK
field transport
#ifdef A_ZAGRODA
#define DOLAR if(!KEY_DOK$dok_zapl .or. ROUND(BRUTTO-zaplacono,A_ZAOKR)=0 .or. przelewem=0,'│','$')
#else
#define DOLAR if(!KEY_DOK$dok_zapl .or. ROUND(BRUTTO-val(transport)-zaplacono,A_ZAOKR)=0,'│','$')
#endif
#else
#define DOLAR '│'
#endif

#ifdef A_WE
#define KWOTA DOLAR+tran(if(KEY_DOK$dok_zby,if(doc_brut,BRUTTO,NETTO),warT_ewiD*if(KEY_DOK$dok_rozch,-1,1)) ,WAPICT)+if(przelewem#0,'P',if(czekiem#0,"K",'│'))
#else
#define KWOTA DOLAR+tran(if(doc_brut,BRUTTO,NETTO),WAPICT)+if(przelewem#0,'P',if(czekiem#0,"K",'│'))
#endif

#endif

#ifdef A_KSEF
field nr_ksef
#define D_EF if(binlen(trim(nr_ksef))<35,'│','§')
#else
#define D_EF '│'
#endif
MEMVAR n_f,dd,da,lp,d_o,DOK,SCR,R,nk1,i,nim,il,wa,ce,changed,dok_zewn,;
      nz,MAG_BIEZ,mag_poz,doc_opcja,operator,miar_opcja,is_spec,dok_rozch,;
      dok_fak,doc_brut,dok_zby,dok_zapl,dokumenty

field   data,smb_dow,nr_dowodu,pozycja,nr_zlec,ilosc,dost_odb,rodz_opak,gram,;
      data_dost,nr_faktury,index,nazwa,adres,uwagi,NUMER_KOL,nr_mag,jm,stan,;
      KTO_PISAL,jm_opcja,nr_kpr,sub_dok,zaplacono,wartosc,wart_ewid,kontrahent,;
      wart_vat
#ifdef A_LPNUM
#define D_LP0 str(0,A_LPNUM) //'  0'
#define D_LP1 str(1,A_LPNUM) //'  1'
#define D_LPPUT(x) str(x,A_LPNUM)
#define D_LPVAL(x) val(x)
#define D_LPSTR(x) str(D_LPVAL(x),3)
#define D_LPSTR1(x) str(D_LPVAL(x),1)
#else
#define D_LP0 '0'
#define D_LP1 '1'
#define D_LPVAL(x) (asc(x)-48)
#define D_LPSTR(x) str(D_LPVAL(x),3)
#define D_LPSTR1(x) x
#define D_LPPUT(x) chr(x+48)
#endif

#ifdef A_OLZA
field stano_kosz,konto_kosz
#endif

#ifdef A_FA
field przelewem,czekiem
#endif

#ifdef A_SUBDOK
  #ifdef A_KHNUM
    #define STDTOP2 "Nr/Lp┬──┬"+PADC("Firma",A_NRLTH,"─")+"┬─Data┬"
    #define DBEG 16+A_NRLTH
  #else
    #define STDTOP2 "Nr/Lp┬──┬─Data┬"
    #define DBEG 15
  #endif
#else
  #ifdef A_KHNUM
   #ifdef A_KTM
#define DBEG 12+A_NRLTH
#define STDTOP2 "Nr/L┬"+PADC("Firma",A_NRLTH,"─")+"┬─Data┬"
   #else
#define STDTOP2 "Nr/Lp┬"+PADC("Firma",A_NRLTH,"─")+"┬─Data┬"
#define DBEG 13+A_NRLTH
   #endif
  #else
   #ifdef A_KTM
#define STDTOP2 "Nr/p┬─Data┬"
#define DBEG 11
   #else
#define STDTOP2 "Nr/Lp┬─Data┬"
#define DBEG 12
   #endif
  #endif
#endif

#ifdef A_SHORTIND
   #define ZLBEG 32
   #define ILPIC 8,ILDEC
   #define STDPOZ1 smb_dow+nr_dowodu+'/'+str(D_LPVAL(pozycja),2)
   #define STDPOZ2 STDPOZ1
   #define STDZLE nr_zlec
           #ifdef A_ZLEC11
              #undef STDZLE
              #define STDZLE pad(nr_zlec,11)
              #define STDBG 44
              #define STDTOP "Nr/Lp┬─Data┬──Ilość─┬Jedn┬─Inf. dod.─┬MG┬─Kod┬Materiał:"
           #else
   #define STDTOP "Nr/Lp┬─Data┬──Ilość─┬Jedn┬Konto─┬MG┬─Kod┬Materiał:"
   #define STDBG 39
           #endif
   #else
   #ifdef A_KTM
     #define STDPOZ1 smb_dow+nr_dowodu+'/'+D_LPSTR1(pozycja)
     #ifdef A_OBR
       #define STDPOZ2 smb_dow+nr_dowodu+if(data<=DatY->d_z_mies1 .or. kto_pisal=operator .or. kto_pisal#HB_UTF8CHR(0x00A0).and.!is_spec,"/","*")+D_LPSTR1(pozycja)
       #define ILPIC 8,ILDEC
       #define STDTOP "Nr/p┬─Data┬──Ilość─┬Jedn┬───Koszty───────┬MG┬─Kod materiału──┬Materiał:"
       #define STDZLE tran(nr_zlec,"@R XXX/XXX/XX/XXXXX")
       #define ZLBEG 31
       #define STDBG 48
     #else
       #define STDPOZ2 STDPOZ1
       #define ILPIC 9,ILDEC
       #define STDTOP "Nr/p┬─Data┬───Ilość─┬Jedn┬Konto─┬MG┬─Kod materiału──┬Materiał:"
       #define STDZLE nr_zlec
       #define ZLBEG 32
       #define STDBG 39
     #endif
   #else
     #define STDZLE nr_zlec
     #define ZLBEG 32
     #ifdef A_OLZA
        #define STDTOP "Nr/p┬─Data┬───Ilość─┬Jedn┬Zlecenie─┬MG┬─Kod materiału┬Materiał:"
        #undef STDTOP2
        #define STDTOP2 "Nr/p┬─Data┬Ko.ko┬─St.Ko┬Wartość───┬"
        #define STDPOZ1 smb_dow+nr_dowodu+'/'+D_LPSTR1(pozycja)
        #define STDPOZ2 STDPOZ1
        #define STDBG 42
        #define ILPIC 9,ILDEC
     #else
        #define ILPIC 8,ILDEC
        #define STDPOZ1 smb_dow+nr_dowodu+'/'+str(D_LPVAL(pozycja),2)
        #ifdef A_ZATW
        #define STDPOZ2 smb_dow+nr_dowodu+if(data<=DatY->d_z_mies1 .or. operator=kto_pisal .or. kto_pisal#HB_UTF8CHR(0x00A0).and.!is_spec,"/","*")+str(D_LPVAL(pozycja),2)
        #else
        #define STDPOZ2 STDPOZ1
        #endif
        #ifdef A_SWW
           #ifdef A_ZLEC11
              #undef STDZLE
              #define STDZLE pad(nr_zlec,11)
              #define STDBG 44
              #define STDTOP "Nr/Lp┬─Data┬──Ilość─┬Jedn┬─Inf. dod.─┬MG┬Symbol┬Materiał:"
           #else
              #define STDBG 39
              #define STDTOP "Nr/Lp┬─Data┬──Ilość─┬Jedn┬Konto─┬MG┬Symbol┬Materiał:"
           #endif
        #else
           #ifdef A_ZLEC11
              #undef STDZLE
              #define STDZLE pad(nr_zlec,11)
              #define STDBG 44
              #define STDTOP "Nr/Lp┬─Data┬──Ilość─┬Jedn┬─Inf. dod.─┬MG┬─Kod mater.─┬Materiał:"
           #else
              #define STDBG 39
              #define STDTOP "Nr/Lp┬─Data┬──Ilość─┬Jedn┬Konto─┬MG┬─Kod mater.─┬Materiał:"
           #endif
        #endif
     #endif
   #endif
#endif
           #ifdef A_SUBDOK
             #undef STDPOZ2
             #define STDPOZ2 smb_dow+nr_dowodu+'/'+str(D_LPVAL(pozycja),2)+"│"+sub_dok
           #endif

STATIC proc set_all(_s,doc_opcja,_skey,stat_ord,init,chgbase)
local a,b,c

if chgbase#NIL
   chgbase:=KEY_DOK+nr_dowodu
endif
if init=.t.
      SELECT FIRMY
            SET ORDER TO TAG FIRM_NUM
      SELECT INDX_MAT
            set relation to
            SET ORDER TO TAG INDX_NUM
      SELECT main
#ifndef STANY
            SET RELATION TO index INTO indx_mat
#else
            SET RELATION TO NR_MAG+index INTO indx_mat
#endif
      select DM
         set relation to
if doc_opcja
         _snagl:=STDTOP2
#ifdef A_KHSEP
#define D_KH kontrahent
#define D_KH1 dost_odb
#else
#define D_KH left(dost_odb,A_NRLTH)
#define D_KH1 if(val(dost_odb)=0,dost_odb,subs(dost_odb,A_NRLTH+2))
#endif
#ifdef A_KHNUM
#define D_KH2 D_KH+'│'
#else
#define D_KH2
#endif

#ifdef A_OLZA
  #ifdef A_VAT
           _snagl+="───VAT────┬"
           _sprompt:={||STDPOZ2+'│'+DTOV(data)+'│'+konto_kosz+"│"+stano_kosz+'│'+tran(warT_ewiD*if(KEY_DOK$dok_rozch,-1,1),WAPICT)+"│"+tran(warT_vaT,WAPICT)+"│"+D_KH1}
  #else
           _sprompt:={||STDPOZ2+'│'+DTOV(data)+'│'+konto_kosz+"│"+stano_kosz+'│'+tran(warT_ewiD*if(KEY_DOK$dok_rozch,-1,1),WAPICT)+"│"+D_KH1}
  #endif
#else
  #ifdef A_FA
   #ifdef A_KPR
           _snagl+="NrKPR┬Nr faktury───┬Wartość───┬"
           _sprompt:={||STDPOZ2+'│'+D_KH2+DTOV(data)+'│'+nr_kpr+D_EF+pad(nr_faktury,13)+KWOTA+D_KH1}
   #else
           _snagl+="Nr faktury───┬Wartość───┬"
           _sprompt:={||STDPOZ2+'│'+D_KH2+DTOV(data)+D_EF+pad(nr_faktury,13)+KWOTA+D_KH1}
   #endif
  #else
#ifdef A_F9
           _snagl+="Nr faktury───┬"
           _sprompt:={||STDPOZ2+'│'+D_KH2+DTOV(data)+D_EF+pad(nr_faktury,13)+'│'+D_KH1}
#else
    #ifdef A_VAT
           _snagl+="Nr faktury───┬───VAT────┬"
           _sprompt:={||STDPOZ2+'│'+D_KH2+DTOV(data)+D_EF+pad(nr_faktury,13)+"│"+tran(warT_vaT,WAPICT)+'│'+D_KH1}
    #else
      #ifdef A_WE
           _snagl+="Nr faktury───┬Wartość───┬"
           _sprompt:={||STDPOZ2+'│'+D_KH2+DTOV(data)+D_EF+pad(nr_faktury,13)+'│'+tran(warT_ewiD*if(KEY_DOK$dok_rozch,-1,1),WAPICT)+'│'+D_KH1}
      #else
           _snagl+="Nr faktury───┬"
           _sprompt:={||STDPOZ2+'│'+D_KH2+DTOV(data)+D_EF+pad(nr_faktury,13)+'│'+D_KH1}
      #endif
    #endif
#endif
  #endif
#endif
#ifdef A_VAT
           _sfor:={||pozycja#D_LP0.or. warT_vaT#0}
#else
           _sfor:={||pozycja#D_LP0}
#endif
#ifdef A_DOKCOLOR
         DEFAULT _scol2 TO min(maxcol(),_scol1+1+binlen(eval(_sprompt,0,_s,.t.)))
         a:=_sprompt
         _sprompt:={|d,_s,z,x|x:=eval(a,d,_s,z),if(z=.t.,x,(devout(left(x,_scol2-_scol1-1),A_DOKCOLOR ),''))}
#endif
else
         set order to 1
         SET RELATION TO D_KH INTO FIRMY
      SELECT main
            set order to TAG MAIN_NRK
         _snagl:=STDTOP
         _sfor:=NIL

#ifdef A_JMO
#define smiaR(x,y,z) if(miar_opcja,if(x%indx_mat->przel=0,str(x/indx_mat->przel,y-4)+"    ",stuff(str(int(x/indx_mat->przel)+x%indx_mat->przel/1000,y,3),y-3,1,"r")),str(x,y,z))
#define imiaR(x,y) if(miar_opcja,y,x)
#else
#define smiaR(x,y,z) str(x,y,z)
#define imiaR(x,y) x
#endif

         _sprompt:={|lam,il|lam:=i_lam(data),STDPOZ1+'│'+;
      DTOV(data)+'│'+smiaR(ilosc*if(KEY_DOK$dok_rozch,-1,1),ILPIC)+'│'+;
      imiaR((lam)->jm,(lam)->jm_opcja)+'│'+STDZLE+'│'+nr_mag+'│'+tran(index,"@R "+ INDEXPIC )+;
      if(alias(lam)#"INDX_MAT",'|','│')+(lam)->nazwA}

#undef imiaR
#undef smiaR
endif
endif
      if doc_opcja
            _spform={|p,l|right(p,l)}
#ifdef A_OLZA
      DO CASE
         case _skey=1 .and. stat_ord#2
              _slth:=0
              _spocz:=D_MM
              _sbeg=1
         case _skey=2
              _slth:=0
              _spocz:=KEY_DOK
              _sbeg=17
         case _skey=3
              _slth:=0
              _spocz:=KEY_DOK
              _sbeg:=22
      endcase
#else
#ifdef A_DMDATA
      if _skey=1 .and. stat_ord#2
              _slth:=0
              _spocz:=if(UPPER(IndexkeY(_skey))='NR_MAG',nr_mag,'')
              _sbeg=1
      elseif _skey=2
              _slth:=0
              _spocz=D_MM + str(year(data),4)
              _sbeg=DBEG
#ifdef A_KPR
              _spform={|p,l|TranR(right(p,l),"##.##|#####")}
#else
              _spform={|p,l|TranR(right(p,l),"##.##")}
#endif
#ifdef A_KHNUM
    elseif _skey=3
       _slth:=0
       _spocz:=if(UPPER(IndexkeY(_skey))='NR_MAG',nr_mag,'')
       _spform:={|p,l|TranR(right(p,l),repl("X",A_NRLTH)+"|##.##")}
       _sbeg:=DBEG-A_NRLTH-1
#ifdef A_KHNAZ
    elseif _skey=4
              _slth:=0
              _spform:={|p,l|right(p,l)}
              _sbeg:=DBEG+A_KHNAZ
              _spocz:=if(UPPER(IndexkeY(_skey))='NR_MAG',nr_mag,'')
#ifdef A_DOKFAK
    elseif _skey=5
              _slth:=0
              _spform:={|p,l|right(p,l)}
#ifdef A_KPR
              _sbeg:=DBEG+6+6
#else
              _sbeg:=DBEG+6
#endif      
              _spocz:=if(UPPER(IndexkeY(_skey))='NR_MAG',nr_mag,'')
#endif
#endif
#endif
      endif
#else
      if stat_ord#2
         _slth:=0
         _spocz:=D_MM
         _sbeg:=1
      endif
#endif
#endif
      if chgbase#NIL
         set order to 1
         seek chgbase
      endif
      else
      DO CASE
         case _skey=1
            _slth:=2
            _spocz:=nr_mag
            _spform:={|p,l|TranR(right(p,l),"##|"+ INDEXPIC +"|XXXX.XX.XX")}
            _sbeg:=STDBG
         case _skey=2 .and. stat_ord#1
            _slth:=0
            _sbeg:=1
            _spocz:=D_MM
            _spform={|p,l|right(p,l)}
#ifndef A_NOMZ
         case _skey=3
            _sbeg=ZLBEG
            _slth:=0
#ifdef A_OLZA
           _spocz=D_MM
           _spform={|p,l|TranR(right(p,l),"XXXXXXXXX|XX|"+ INDEXPIC +"|XXXX.XX.XX")}
#else
           _spocz:=""
#ifdef A_ZLEC11
           _spform={|p|TranR(p,"XXXXXXXXXXX|##|"+ INDEXPIC +"|XXXX.XX.XX")}
           //_spform={|p|TranR(p,"XXXXXXXXXXX|")+TRANR(subs(p,binlen(nr_zlec)+1),"##|"+ INDEXPIC +"|XXXX.XX.XX")}
#else
           _spform={|p|TranR(p,"XXXXXX|##|"+ INDEXPIC +"|XXXX.XX.XX")}
#endif
#endif
#endif
      endcase
      if chgbase#NIL
         set order to 2
         seek chgbase
      endif
      endif
      SET ORDER TO (_skey)
      _swar=&('{|p|'+IndexkeY(0)+'=p'+'}')

return
*************

func dmprzeg(ent)
local stat_ord
public doc_brut
#ifdef A_LAN
      select daty
      goto 1
#endif

      SETPOS(10,0)

#ifdef A_MYSZ
   #define D_MYSZ ,bx,cx,dx,myszflag
#else
   #define D_MYSZ
#endif
return  szukam({1,0,maxrow(),,,,,,{|k,s D_MYSZ|przeg(k,s,@ent,@stat_ord D_MYSZ)}})

***********************
static FUNCTION przeg(_skey,_s,ent,stat_ord D_MYSZ)

MEMVAR lastlevel
local _menudm,x

DO CASE

   CASE _skey=0
      _snagkol:=5
#ifdef A_MYSZ
      _skproc[14]:=NIL
#endif
      _sfilt:=""
      _sfilb:=NIL
      stat_ord:=if(doc_opcja,2,1)
      _skey:=3-stat_ord
      select main
      set order to TAG MAIN_NRK
#ifdef A_F9
      if ent#NIL
         if ent=0
           doc_opcja:=.t.
#ifdef A_KOB
           _sfilb:={||nr_faktury=n_f}
           _sfilt:='nr_faktury=n_f'
           _skey:=1 //wg numeru
#else
#ifdef A_SPECYF
           doc_opcja:=.f.
           _skey:=3
#else
#ifdef A_DOKFAK
           _skey:=5 //wg zlecenia
#else
           _skey:=3 //wg firmy
#endif
#endif
#endif
         else
           doc_opcja:=.f.
           stat_ord:=1
           _skey:=2 //wg numeru
           goto ent
         endif
      endif
#endif
#ifndef A_MM
      if nr_mag#mag_biez .or. deleted()
#endif
         if eof() .or. deleted()
            skip -1
         endif
#ifndef A_MM
         if nr_mag#mag_biez
            if !dbseek( mag_biez )
               return(.t.)
            endif
         endif
      endif
#endif
      if doc_opcja
         select dm
         set order to 1
         if eof() .or. deleted()
            skip -1
         endif
#ifndef A_MM
         if nr_mag#mag_biez
            SEEK MAIN->NR_MAG+MAIN->SMB_DOW+MAIN->NR_DOWODU
         endif
#endif
      endif
      set_all(_s,doc_opcja,_skey,0,.t.)
#ifdef A_F9
      if ent#NIL
      if ent=0
         _spocz:=if(UPPER(IndexkeY(0))='NR_MAG',nr_mag,'')
#ifdef A_KOB
         _spocz+='PW'
         _slth:=2
#else
#ifdef A_DOKFAK
         _spocz+=UpP(trim(n_f))
         _slth:=binlen(trim(n_f))
#else
         _spocz+=FIRMY->numer_kol
         _slth:=A_NRLTH
#endif
#endif
         seek _spocz+'@'
         skip -1
      else
         _spocz:=KEY_DOK+nr_dowodu
         _slth:=7
      endif
      endif
#endif
      ent:=ent#NIL
      set cursor on

   CASE _skey=-1

      doc_opcja:=!doc_opcja
      _skey:=stat_ord
      stat_ord:=ordnumber()
      _sfilt:=""
      _sfilb:=NIL
      set_all(_s,doc_opcja,_skey,stat_ord,.t.,.t.)
      refresh(,_s)

#ifdef A_FA
   CASE _skey=-2
      if doc_opcja
        doc_brut:=!doc_brut
        refresh(,_s)
      endif
#endif

   CASE _skey=27
      RETURN(.T.)

   case _skey=2 .or. _skey=26
      if doc_opcja
#ifdef A_OLZA
      IF _skey=26
         _skey=(ordnumber()+1)%3+1
      ELSE
         _skey=ordnumber()%3+1
      ENDIF
#else
#ifdef A_DMDATA
#ifdef A_KHNUM
#ifdef A_KHNAZ
#ifdef A_DOKFAK
    IF _skey=26
         _skey={4,3,1,5,2}[ordnumber()]
    ELSE
         _skey={3,5,2,1,4}[ordnumber()]
    ENDIF
#else
    IF _skey=26
         _skey={4,3,1,2}[ordnumber()]
    ELSE
         _skey={3,4,2,1}[ordnumber()]
    ENDIF
#endif
#else
    IF _skey=26
         _skey={2,3,1}[ordnumber()]
    ELSE
         _skey={3,1,2}[ordnumber()]
    ENDIF
#endif
#else
    _skey:=3-ordnumber()
#endif
#else
    return .f.
#endif
#endif

   else
#ifdef A_NOMZ
      _skey:=3-ordnumber()
#else
      IF _skey=26
         _skey=(ordnumber()+1)%3+1
      ELSE
         _skey=ordnumber()%3+1
      ENDIF
#endif
   endif
      set_all(_s,doc_opcja,_skey,0)
      refresh(,_s)

#ifdef A_MYSZ
   case _skey=14
        if bx=1 .and. dx=row() .and. cx=_scol1+7
#ifndef __HARBOUR__
            sysint(51,1)
            sysint(51,10,0,0,7168+18)
            do while bx#0
              sysint(51,3,@bx)
            enddo
            sysint(51,10,0,-1,30464)
            sysint(51,2)
#endif
            przeg(-1,_s,@ent,@stat_ord)

#ifdef A_OLZA
        elseif doc_opcja .and. bx=1 .and. dx=row() .and. cx=_scol1-1
#ifndef __HARBOUR__
            sysint(51,1)
            sysint(51,10,0,0,7168+16)
            do while bx#0
              sysint(51,3,@bx)
            enddo
            sysint(51,10,0,-1,30464)
            sysint(51,2)
#endif
            set order to 3
            przeg(2,_s,@ent,@stat_ord)

        elseif doc_opcja .and. bx=1 .and. dx=row() .and. cx=_scol1+16
            sysint(51,1)
            sysint(51,10,0,0,7168+16)
            do while bx#0
              sysint(51,3,@bx)
            enddo
            sysint(51,10,0,-1,30464)
            sysint(51,2)
            set order to 1
            przeg(2,_s,@ent,@stat_ord)
        elseif doc_opcja .and. bx=1 .and. dx=row() .and. cx=_scol1+21
            sysint(51,1)
            sysint(51,10,0,0,7168+16)
            do while bx#0
              sysint(51,3,@bx)
            enddo
            sysint(51,10,0,-1,30464)
            sysint(51,2)
            set order to 2
            przeg(2,_s,@ent,@stat_ord)
#else
#ifdef A_DMDATA
        elseif doc_opcja .and. bx=1 .and. dx=row() .and. cx=_scol1-1
            sysint(51,1)
            sysint(51,10,0,0,7168+16)
            do while bx#0
              sysint(51,3,@bx)
            enddo
            sysint(51,10,0,-1,30464)
            sysint(51,2)
            set order to 2
            przeg(26,_s,@ent,@stat_ord)
        elseif doc_opcja .and. bx=1 .and. dx=row() .and. cx=DBEG-1
            sysint(51,1)
            sysint(51,10,0,0,7168+16)
            do while bx#0
              sysint(51,3,@bx)
            enddo
            sysint(51,10,0,-1,30464)
            sysint(51,2)
            set order to 1
            przeg(26,_s,@ent,@stat_ord)
#ifdef A_KHNUM
        elseif doc_opcja .and. bx=1 .and. dx=row() .and. cx=DBEG-5
            sysint(51,1)
            sysint(51,10,0,0,7168+16)
            do while bx#0
              sysint(51,3,@bx)
            enddo
            sysint(51,10,0,-1,30464)
            sysint(51,2)
            set order to 2
            przeg(26,_s,@ent,@stat_ord)
#ifdef A_KHNAZ
        elseif doc_opcja .and. bx=1 .and. dx=row() .and. cx=DBEG+A_KHNAZ-1
            sysint(51,1)
            sysint(51,10,0,0,7168+16)
            do while bx#0
              sysint(51,3,@bx)
            enddo
            sysint(51,10,0,-1,30464)
            sysint(51,2)
            set order to 3
            przeg(2,_s,@ent,@stat_ord)
#endif
#endif
#endif
#endif
        elseif !doc_opcja .and. bx=1 .and. dx=row() .and. cx=_scol1-1

            sysint(51,1)
            sysint(51,10,0,0,7168+16)
            do while bx#0
              sysint(51,3,@bx)
            enddo
            sysint(51,10,0,-1,30464)
            sysint(51,2)
            set order to 1
            przeg(2,_s,@ent,@stat_ord)

        elseif !doc_opcja .and. bx=1 .and. dx=row() .and. cx=_scol1+STDBG+1

            sysint(51,1)
            sysint(51,10,0,0,7168+16)
            do while bx#0
              sysint(51,3,@bx)
            enddo
            sysint(51,10,0,-1,30464)
            sysint(51,2)
            set order to 2
            przeg(26,_s,@ent,@stat_ord)
#ifndef A_NOMZ
        elseif !doc_opcja .and. bx=1 .and. dx=row() .and. cx=_scol1+ZLBEG-2

            sysint(51,1)
            sysint(51,10,0,0,7168+16)
            do while bx#0
              sysint(51,3,@bx)
            enddo
            sysint(51,10,0,-1,30464)
            sysint(51,2)
            set order to 2
            przeg(2,_s,@ent,@stat_ord)
#endif
        else
          return mysz(_s,bx,cx,dx,myszflag)
        endif
#endif

   CASE _si=0
   case _skey=13 .and. ent
      if doc_opcja
         select main
         set order to 2
         seek dm->(KEY_DOK+nr_dowodu)
         select dm
      endif   
      return _sret:=.t.

   case _skey=K_INS
      x:=lastlevel
#ifdef A_SUBDOK
      _menudm:="    "
#else
      _menudm:="  "
#endif
      if !aczojs(dokumenty[MAG_POZ],@_menudm,@x)
         return .f.
      ENDIF
      lastlevel:=x

      _skey=ordnumber()
      x:=doc_opcja
      private CHANGED:=.F.
      dok_in(mag_biez,_MENUDM)
      doc_opcja:=x
      set cursor on
      if doc_opcja
      IF _sfilb#NIL
        SET RELATION TO D_KH INTO FIRMY
      ENDIF
      else
      SET RELATION TO D_KH INTO FIRMY
      SELECT main
    IF _sfilb#NIL
#ifndef STANY
          SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO index INTO indx_mat
    ELSE
          SET RELATION TO index INTO indx_mat
#else
          SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO NR_MAG+index INTO indx_mat
    ELSE
          SET RELATION TO NR_MAG+index INTO indx_mat
#endif
    ENDIF
    endif
      SET ORDER TO (_skey)
    IF CHANGED
       REFRESH(,_s)
    ENDIF

   CASE _skey=13
      _skey=ordnumber()
      x:=doc_opcja
      private CHANGED:=.F.
#ifdef A_MM
      DOK_IN(D_MM,smb_dow,nr_dowodu+if(x,'',pozycja), ent )
#else
      DOK_IN(D_MM,smb_dow,nr_dowodu+if(x,'',pozycja), ent .or. mag_biez#D_MM)
#endif
      doc_opcja:=x
      set cursor on
      if doc_opcja
      IF _sfilb#NIL
        SET RELATION TO D_KH INTO FIRMY
      ENDIF
      else
      SET RELATION TO D_KH INTO FIRMY
      SELECT main
    IF _sfilb#NIL
#ifndef STANY
          SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO index INTO indx_mat
    ELSE
          SET RELATION TO index INTO indx_mat
#else
          SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO NR_MAG+index INTO indx_mat
    ELSE
          SET RELATION TO NR_MAG+index INTO indx_mat
#endif
    ENDIF
    endif
      SET ORDER TO (_skey)
    IF CHANGED
       REFRESH(,_s)
    ENDIF

   CASE _skey=asc('+')
      _spocz:=left(_spocz,binlen(_spocz)-1)
      --_slth
      go _srec[_sm]
      _skey=ordnumber()
      x:=doc_opcja
      private CHANGED:=.F.
      DOK_IN(D_MM,smb_dow)
      doc_opcja:=x
      set cursor on
      if doc_opcja
      IF _sfilb#NIL
        SET RELATION TO D_KH INTO FIRMY
      ENDIF
      else
      SET RELATION TO D_KH INTO FIRMY
      SELECT main
    IF _sfilb#NIL
#ifndef STANY
          SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO index INTO indx_mat
    ELSE
          SET RELATION TO index INTO indx_mat
#else
          SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO NR_MAG+index INTO indx_mat
    ELSE
          SET RELATION TO NR_MAG+index INTO indx_mat
#endif
    ENDIF
    endif
      SET ORDER TO (_skey)
      IF CHANGED
       REFRESH(,_s)
      ENDIF
      return .t.

   CASE _SKEY=9 // TAB
        _skey=ordnumber()
        if !doc_opcja
        select dm
        seek  MAIN->(KEY_DOK+NR_DOWODU)
        endif
        private changed:=.f.
        WYDRUK_dok()
        SELECT main
        if doc_opcja
         set relation to
         select dm
        else
        if _sfilb#NIL
#ifndef STANY
           SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO index INTO indx_mat
#else
           SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO NR_MAG+index INTO indx_mat
#endif
        endif
        endif
        SET ORDER TO (_skey)
      if changed
        go _srec[_sm]
        REFRESH LINE _sm+_srow1-1 DIRECTION 0
      endif

   case _skey=-8
       if doc_opcja
        SET RELATION TO D_KH INTO FIRMY
       else
#ifndef STANY
        SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO index INTO indx_mat
#else
        SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO NR_MAG+index INTO indx_mat
#endif
       endif
        _sfil(_s)
        IF _sfilb=NIL
        if doc_opcja
          set relation to
          else
#ifndef STANY
          SET RELATION TO index INTO indx_mat
#else
          SET RELATION TO NR_MAG+index INTO indx_mat
#endif
        endif
        ENDIF   

   case _skey=-9
       if doc_opcja
        SET RELATION TO D_KH INTO FIRMY
       else
#ifndef STANY
        SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO index INTO indx_mat
#else
        SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO NR_MAG+index INTO indx_mat
#endif
       endif
        _slist(,_s)
        IF _sfilb=NIL
        if doc_opcja
          set relation to
          else
#ifndef STANY
          SET RELATION TO index INTO indx_mat
#else
          SET RELATION TO NR_MAG+index INTO indx_mat
#endif
        endif
        ENDIF   

   case _skey=-6
      if doc_opcja
        SET RELATION TO D_KH INTO FIRMY
#ifdef A_OLZA
        do case
           case ordnumber()=1
                _slist(".\hn*.frm",_s)
           case ordnumber()=2
                _slist(".\hk*.frm",_s)
           case ordnumber()=3
                _slist(".\hs*.frm",_s)
        endcase
#else
#ifdef A_DMDATA
        if ordnumber()=1
           _slist(".\hn*.frm",_s)
#ifdef A_KHNUM
        elseif ordnumber()=3
           _slist(".\hf*.frm",_s)
        elseif ordnumber()=4
           _slist(".\hf*.frm",_s)
        elseif ordnumber()=5
           _slist(".\hn*.frm",_s)
#endif
        else
           _slist(".\hd*.frm",_s)
        endif
#else
        _slist(".\h*.frm",_s)
#endif
#endif
        IF _sfilb=NIL
          SET RELATION TO
        ENDIF   
        else
#ifndef STANY
        SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO index INTO indx_mat
#else
        SET RELATION TO KEY_DOK+NR_DOWODU INTO DM, TO NR_MAG+index INTO indx_mat
#endif
        do case
           case ordnumber()=1
                _slist(".\di*.frm",_s)
           case ordnumber()=2
                _slist(".\dn*.frm",_s)
           case ordnumber()=3
                _slist(".\dz*.frm",_s)
        endcase
        IF _sfilb=NIL
#ifndef STANY
          SET RELATION TO index INTO indx_mat
#else
          SET RELATION TO NR_MAG+index INTO indx_mat
#endif
        ENDIF
        endif
#ifdef A_JMO
   case _skey=K_F8 .and. !doc_opcja //F8
      miar_opcja=!miar_opcja
      aeval(_srec,{|x,i|dbgoto(x),setpos(_srow1+i-1,_scol1),dispout(padr(eval(_sprompt,0,_s),_scol2-COL()))},1,_si)
      SAVE LINE _sm+_srow1-1
#ifdef A_LAN
      go _srec[_sm] // onerefresh off
#endif
#endif
ENDCASE

RETURN(.F.)
*************
