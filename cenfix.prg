//hbmk2 -prgflag=-dSIMPLE -gtnul -u+polok.ch cenfix io io_2 error


#command ? [<List,...>] => ?? HB_EOL()[;?? <List>]
#command P [<List,...>] => PP HB_EOL()[;PP <List>]
#command PP [<xn,...>] => [QQ <xn>]
#command Q [<List,...>] => QQ HB_EOL()[;QQ <List>]

#ifdef SIMPLE

#ifdef __PLATFORM__UNIX
#command ?? [<xn,...>] => [OuterR(HB_TRANSLATE(TRAN(<xn>,),,'UTF8'))]
#command QQ [<xn,...>] => [OutstD(HB_TRANSLATE(TRAN(<xn>,),,'UTF8'))]
#else
#command ?? [<xn,...>] => [OuterR(<xn>)]
#command QQ [<xn,...>] => [OutstD(<xn>)]
#endif

#command ACCEPT TO <idVar>                                              ;
      => <idVar> := StrTran( FReadStr(0, 256), HB_EOL() )
#else      
#command ?? [<xn,...>] => [OuterR(<xn>)]
#command QQ [<xn,...>] => [OutstD(<xn>)]

#endif

#command EJECT => QQ chr(12)
#command ACCEPT <cPrompt> TO <idVar>                                    ;
      => ? <cPrompt>                                                    ;
       ; ACCEPT TO <idVar>


#ifdef A_DIETA
  #ifndef A_MAGDI
    #define A_MAGDI " 1"
  #endif
#endif
#ifdef A_MM
#define KEY_DOK smb_dow
#else
#define KEY_DOK nr_mag+smb_dow
#endif

external padl,strtran

field ilosc,index,nazwa,jm,data,wartosc,nr_mag,stan,ewidencja,;
      data_zmian,smb_dow,nr_dowodu,pozycja,nr_zlec,KTO_PISAL,p_r,;
      WART_MIES1,ZAMK_MIES1,WART_MIES2,ZAMK_MIES2,wart_roku,zamkn_roku,;
      cena_przy

memvar dok_rozch,defa,firma_n

#ifdef A_LAN
#ifndef A_DDBF
  #define A_DDBF
#endif
#endif
#ifndef UpP
request upp
#endif

proc main(jaki_mag,jakie_dok,od,ans)

local key,ws,is,cs,mprint:=.f.,il,wa,a,b,c,d,rcrd,doknfa,da,daINI,r,mag_poz,;
    x,y,z,odc

local dokumenty,magazyny
field numer,smb_dok
#ifndef A_DDBF
memvar  d_z_mies1,d_z_mies2,d_z_rok,data_gran
private d_z_mies1,d_z_mies2,d_z_rok,data_gran
#define DatY MEMVAR
#endif

private dok_rozch:=""

public defa:=getenv("MAGDEF")

?
? "Program poprawia ceny ewidencyjne dokument¢w rozchodu."
?

if od=NIL //'  .  .    '
   ACCEPT "Prosz© podaÜ, dat© od kt¢rej naleæy poprawiac dokumenty " TO od
endif

od:=ctod(od)

IF ""#defa .and. right(defa,1)<>HB_ps()
   defa+=HB_ps()
endif
 
if ""=defa
  SET PATH TO ("."+HB_ps())
else
  SET PATH TO ("."+HB_ps()+HB_OsPathListSeparator()+defa)
endif

SET DEFAULT TO (defa+"roboczy"+HB_ps())

DO WHILE .T.

   #define D_Z1 max(DatY->d_z_mies1,DatY->data_gran)
   #define D_Z2 DatY->d_z_mies2
   #define D_Z3 DatY->d_z_rok

#ifdef A_DDBF
   use daty readonly
#else
   x:="daty.ini";do while inirest(@x);(&x,x:=NIL);enddo
#endif

   IF !EMPTY(od) .and. od>=D_Z3

   ELSEIF KEY=NIL
      IF D_Z1=D_Z3 .and. file(defa+str(year(D_Z3),4)+HB_ps()+"daty.*")
         SET DEFAULT TO (defa+str(year(D_Z3),4)+HB_ps() )
         KEY:=D_Z3
         LOOP
      ENDIF
   ELSEIF D_Z1=KEY
      SET DEFAULT TO (defa+"roboczy"+HB_ps() )
      ++KEY
      LOOP
   ENDIF

   EXIT
ENDDO

      reuse()

#ifdef A_DIETA
if jaki_mag=NIL
   jaki_mag:=A_MAGDI
   if jakie_dok=NIL
      jakie_dok:=""
   endif
endif
#else
if jaki_mag=NIL
   jaki_mag=""
//   ACCEPT "Prosz© podaÜ, dla jakiego magazynu dokonaÜ korekty " TO jaki_mag
endif
#endif

if jakie_dok=NIL
   ?
   ACCEPT "Prosz© podaÜ, dla jakich dokument¢w dokonaÜ korekty " TO jakie_dok
endif
jakie_dok:=UPPER(jakie_dok)
magazyny:={}
dokumenty:={}
sel('magazyny')
EXECUTE {||aadd(MAGAZYNY,NUMER+" "+nazwa),aadd(dokumenty,{})}
USE
sel('dok_def')
  DO WHILE !EOF()
    mag_poz:=ascan(magazyny,nr_mag)
    if mag_poz=0
      r:=len(magazyny)
      for mag_poz=1 to r
        if 0=ascan(dokumenty[mag_poz],smb_dok)
          aadd(dokumenty[mag_poz],SMB_DOK)
          if p_r#"P"
             dok_rozch+=left(magazyny[mag_poz],2)+SMB_DOK+","
          endif
        endif
      next
    ELSE
      if 0=(r:=ascan(dokumenty[mag_poz],smb_dok))
         aadd(dokumenty[mag_poz],smb_dok)
         if p_r#"P"
             dok_rozch+=left(magazyny[mag_poz],2)+SMB_DOK+","
         endif
      endif
    ENDIF
    SKIP
  ENDDO
if ""=jakie_dok
   dokumenty:={}
   go top
   DO WHILE !EOF()
      if nr_mag="  ".and. 0=ascan(dokumenty,smb_dok) .or. nr_mag=" 1"
         aadd(dokumenty,SMB_DOK)
#ifdef A_FA
         if wartosc#"+" .and. ewidencja#"Z" .AND. p_r#"P"
#else
         if wartosc#"+" .AND. P_R#"P"
#endif
            jakie_dok+=","+smb_dok
         endif
      ENDIF
      SKIP
   ENDDO
endif
  use
          IF empty(od)
          od := D_Z1+1
          ENDIF
          ?
          ? "PRZERWANIE PROGRAMU - CTRL+C"
          ?
          IF ""=JAKI_MAG
             ? "DOKUMENTY "+jakie_dok+" OD "+dtoc(od)
          ELSE
             ? "DOKUMENTY "+jakie_dok+" MAGAZYNU "+JAKI_MAG+" OD "+dtoc(od)
          ENDIF
          ?
          if empty(ans)
             ACCEPT "CZY POPRAWIAè CENY DOKUMENT‡W OD "+dtoc(od)+" (Tak/Nie) ? " to ans
          endif
          
          if !"T"$UPPER(ans)
             quit
          ENDIF


QQ padr(firma_n,64)+" dnia "+dtoc(date())
Q
Q "Raport poprawy cen dokument¢w: "+jakie_dok
Q
IF ""#JAKI_MAG
Q "Magazyn: "+jaki_mag
Q
ENDIF
P "DOKUMENT                   PRZED POPRAW§                  PO POPRAWIE"
P
select STANY
seek jaki_mag
locate for data_zmian>=od rest
do while found().and.nr_mag+index=jaki_mag
   lock message "UWAGA!: Nie potrafi© uzyskaÜ wyà•cznego dost©pu do stanu: "+nr_mag+"/"+index
   ws:=if(D_Z1=D_Z2,if(D_Z1=D_Z3,wart_roku,wart_mies2),wart_mies1)
   Is:=if(D_Z1=D_Z2,if(D_Z1=D_Z3,zamkn_roku,zamk_mies2),zamk_mies1)
   cs:=if(is=0 ,cena_przy,ws/is)
   ?? chr(13)+index
#ifdef A_FIFO
   odc:=STANY->cena_przy
   if STANY->(fieldpos('stanx'))<>0
     STANY->validx:=.f.
   endif
#endif
   select main
   seek STANY->nr_mag + STANY->index+dtos(D_Z1+1)
   DBEVAL({||ws+=WARTOSC,is+=ILOSC,if(smb_dow$jakie_dok .or. ILOSC<=0,cs:=WARTOSC/ILOSC,odc:=WARTOSC/ILOSC)},,{||nr_mag+index=STANY->nr_mag+STANY->index .and. data<od})
   if is<>0
      cs:=ws/is
   endif
   ws:=ROUND(ws,A_ZAOKR)
   is:=ROUND(is,3)
   do while nr_mag+index=STANY->nr_mag+STANY->index
#ifdef A_ZAGRODA
      if smb_dow='UB' .and. nr_zlec='U'
	a:=recno()
	b:=is
	c:=data
	dbeval({||b+=ilosc},,{||nr_mag+index=STANY->nr_mag+STANY->index .and. data=c})
	dbgoto(a)
	IF round(b - val(strtran(subs(nr_zlec,2),' ')),3)<>0
	   lock in main	
	   STANY->stan-=main->ilosc
	   main->ilosc+=val(strtran(subs(nr_zlec,2),' '))-b
	   STANY->stan+=main->ilosc
	   unlock in main	
	endif
      endif
#endif
      IF CS<0
        p
        matprint()
        p smb_dow+nr_dowodu+"/"+pozycja+' '+DTOC(DATA)+' '+str(ilosc)+' '+indx_mat->jm
        P 'CENA ZAPASU UJEMNA: '+str(CS)+' '+indx_mat->jm
        P
        CS:=0
      ENDIF
      if smb_dow$jakie_dok
#ifdef A_FIFO
         b:=ws
         a:=is
         il:=-ilosc
         if round(is-il,3)=0 .or. il=0

         elseif is<il //.and. is>=0 
           //cs:=odc // ostatnia dobra cena jaka by nie byàa
           if odc#0
              cs:=max(0,((il-is)*odc+ws)/il)
           endif
         else
           rcrd:=recno()
           da:=data
           do while .t.
             skip -1
#ifdef A_LIFO
             if BOF() .or. nr_mag+index<STANY->nr_mag+STANY->index
                exit
             endif
             a-=ilosc
             b-=wartosc
             if ilosc<0 .or. wartosc=0 .and. ilosc=0 .or. il<0 .and. (wartosc=0 .or. ilosc=0 .or. round(is-a,3)<=0)
                loop
             endif
             if round(a+il-is,3)<=0
                c:=max(is-a-ilosc,0)
                d:=max(ws-b-wartosc,0)
                if round(is-a-c,3)=0
                   cs:=d/il
                else
                   cs:=(d-(c-il)*(ws-b-d)/(is-a-c))/il
                endif
                exit
             endif
           enddo
#else
//----------------------------
             if (bof() .or. nr_mag+index<STANY->nr_mag+STANY->index) .and. round(a-il,3)>0 //rozchod nie bierze caàego stanu pocz
               y:=str(year(data)-1,4)
               x:=select('main'+y)
               if x=0
                 x:=select()
                 begin sequence
                   if !file(defa+y+HB_ps()+'main.dbf')
                     break
                   endif
                   nuse (defa+y+HB_ps()+'main') new alias ('main'+y)
#ifdef A_CDX
                   set order to tag main_ind
                   if empty(indexord())
#else
                   if file(defa+y+HB_ps()+'main_ind'+ordbagext())
                      set index to (defa+y+HB_ps()+'main_ind')
                   else
#endif
                      break
                   endif
                   x:=nil
                 recover
                   cs:= max(0,b/a)
#ifdef A_CEOKR
                   cs:= max(0,max(Round(cs-1,0),min(round(cs+1,0),(b-(a-il)*Round(cs,A_ZAOKR))/il)))
#endif
                   exit
                 end sequence
               else
                 select (x)
                 x:=NIL
               endif
               if x=NIL
                 set order to tag main_ind
                 seek STANY->nr_mag+STANY->index+"@"
                 loop
               endif
             endif
//----------------------------
             if BOF() .or. nr_mag+index<STANY->nr_mag+STANY->index .or. ilosc=0 .and. wartosc<>0
                cs:= max(0,b/a)
#ifdef A_CEOKR
                cs:= max(0,max(Round(cs-1,0),min(round(cs+1,0),(b-(a-il)*Round(cs,A_ZAOKR))/il)))
#endif
                exit
             elseif smb_dow$jakie_dok .or. (ilosc=0 .and. wartosc=0)
                loop
             endif
             a-=ilosc
             b-=wartosc
             if round(a-il,3)<=0 // po odj©ciu PZ stan =< ilosci
                odc:=wartosc/ilosc
                cs:=(b+(il-a)*odc)/il  // wartosc := to ile pozostaào pieniedzy dodaÜ to ile pozyczamy z tego pz
#ifdef A_CEOKR
                c:=Round(odc,A_ZAOKR)
                d:=wartosc-c*ilosc //tyle dodac do warto rozch w ramach korekty
                cs:=max(max(0,Round(cs-1,0)),min(round(cs+1,0),(b+d+(il-a)*c)/il))
#endif
                exit
             endif
           enddo
#endif
           select main
           go rcrd
           if cs<0
              cs:=0
           elseif ws>=0 .and. is>=0 .and. ws-cs*il<0 .and. is-il>=0
              cs:=ws/is
           endif 
         endif
#else
         if (il:=is+ilosc)<0
            key:=recno()
            wa:=0
            locate for !smb_dow$jakie_dok .and. ilosc>0 .and. wartosc>0 .and. (wa+=wartosc,il+=ilosc)>=0 rest while nr_mag+index=STANY->nr_mag+STANY->index
            if found()
               goto key
               il-=is+ilosc
               if (cs:=wa/il)<0
                  cs:=0
               endif
            else
               goto key
            endif
         endif
#endif
#ifdef A_KASA
         if ROUND(round(ilosc*cs,A_ZAOKR)-wartosc,A_ZAOKR)#0 .AND. CS>=0 .or. smb_dow="M"
#else
         if ROUND(round(ilosc*cs,A_ZAOKR)-wartosc,A_ZAOKR)#0 .AND. CS>=0
#endif
            dofix(ROUND(ilosc*cs,A_ZAOKR))
         endif
      endif
      ws:=ROUND(ws+wartosc,A_ZAOKR)
      is:=round(is+ilosc,3)
      if round(is,3)#0
         cs:=ws/is
      endif
      IF is<0
        p
        matprint()
        p smb_dow+nr_dowodu+"/"+pozycja+' '+DTOC(DATA)+' '+str(ilosc)+' '+indx_mat->jm
        P 'STAN UJEMNY: '+str(is,10,3)+' '+indx_mat->jm
        P
      ELSEIF ws<0
        P
        matprint()
        p smb_dow+nr_dowodu+"/"+pozycja+' '+DTOC(DATA)+' '+str(ilosc)+' '+indx_mat->jm
        P 'WARTOóè ZAPASU UJEMNA: '+str(ws)
        P
      ELSEIF ws#0 .AND. is=0
        P
        matprint()
        p smb_dow+nr_dowodu+"/"+pozycja+' '+DTOC(DATA)+' '+str(ilosc)+' '+indx_mat->jm
        P 'WARTOóè STANU ZEROWEGO R‡ΩNA OD ZERA: '+str(ws)
        P
      endif
      skip
   enddo

   if ws<>0 .and. (is=0 .or. cs<0)
     skip -1
     do while nr_mag+index=STANY->nr_mag+STANY->index .and. data>=od .and. .not. smb_dow$jakie_dok
       skip -1
     enddo
     if nr_mag+index=STANY->nr_mag+STANY->index .and. data>=od .and. smb_dow$jakie_dok
        dofix(ROUND(wartosc-ws,A_ZAOKR))
        ws:=0
     endif
   endif

   select STANY
   if ROUND(wartosc-WS,A_ZAOKR)#0 .or. ROUND(stan-IS,3)#0
        P
        matprint()
        P "NIEZGODNY STAN KO„COWY "+str(stan,10,3)+' '+indx_mat->jm+' '+str(wartosc/stan,10,2)+' '+str(wartosc)
         wartosc:=ws;stan:=is
        p "POPRAWA NA"+' '+str(stan,10,3)+' '+indx_mat->jm+' '+str(wartosc/stan,10,2)+' '+str(wartosc,10,2)
        P
   endif
#ifdef A_LAN
   dbunlock()
   main->(dbunlock())
#endif
   continue
   //skip
enddo

p "KONIEC."
eject


return
**********
proc dofix(w)
  local a,b,c,d
  matprint()
  pp smb_dow+nr_dowodu+"/"+pozycja+' '+DTOC(DATA)+' '+str(ilosc)+' '+indx_mat->jm
  p "POPRAWA WARTOóCI DOKUMENTU: "+str(wartosc/ilosc)+' '+str(wartosc)

  lock in main
  STANY->wartosc+=w-wartosc
#ifdef A_WE
#ifdef A_LAN
  if dm->(dbseek(main->(KEY_DOK+nr_dowodu),.f.) .and. reclock(.t.))
#else
  if dm->(dbseek(main->(KEY_DOK+nr_dowodu),.f.))
#endif           
    DM->warT_ewiD+=w-wartosc
#ifdef A_LAN
    dm->(dbunlock())
#endif
  endif
#else
#ifdef A_KASA
  dm->(dbseek(main->(KEY_DOK+nr_dowodu),.f.))
#endif
#endif
  wartosc:=w
  pp " NA "+str(wartosc/ilosc)+' '+str(wartosc)
  p
#ifdef A_KASA
  a:=recno() // MM druga poàowka
  c:=-ilosc
  d:=-wartosc
  ORDSETFOCUS("MAIN_NRK")
  IF dbseek(left(dm->nr_faktury,9)+pozycja)
     if index=STANY->index.and.data=DM->data.and.round(ilosc-c,3)=0
        lock
        b:=STANY->(recno())
        STANY->(dbseek(main->nr_mag+main->index,.f.))
        lock STANY->(recno()) IN STANY
        STANY->wartosc+=d-wartosc
        wartosc:=d
#ifdef A_LAN
        STANY->(dbrunlock(recno()))
#endif
        STANY->(dbgoto(b))
     else
        pp "NIEZGODNY DOKUMENT DRUGIEJ STRONY: "+left(dm->nr_faktury,9)+pozycja
        p
     endif
  endif
  ORDSETFOCUS("MAIN_IND")
  dbgoto(a)
#endif
  UNLOCK IN MAIN

return
**********
proc matprint
static mprint:=""
         if mprint#STANY->(nr_mag+index)
#ifndef A_CDX
#ifndef STANY
            indx_mat->(dbseek(STANY->index,.f.))
#endif
#endif
            p
            p STANY->nr_mag+"/"+STANY->index+' '+indx_mat->nazwa
            p
            mprint:=STANY->(nr_mag+index)
         endif
return
***************************************
func wersja 
return '1'
***************************************
proc reuse()
      sel("DM",1)
      sel("MAIN","MAIN_IND")
      sel("INDX_MAT","INDX_NUM")
#ifndef STANY
      sel("STANY","STAN_MAG")
      set relation to index into indx_mat
#endif
return
***************************************
