#include "inkey.ch"
#include "set.ch"

//#ifdef __PLATFORM__UNIX
//#define nl chr(10)
//#else
#define nl chr(13)+chr(10)
//#endif

#define D_TIMEOUT 5000

#ifdef A_MM
#define KEY_DOK smb_dow
#define D_MM 7
#else
#define KEY_DOK nr_mag+smb_dow
#define D_MM 9
#endif

#ifdef A_DIETA
  #ifndef A_MAGDI
    #define A_MAGDI " 1"
  #endif
#endif

#ifdef A_7
  #ifndef A_WP
  #define A_WP
  #endif
#endif

#ifdef A_KHSEP
#define D_KH kontrahent
#define D_KH1 dost_odb
#else
#define D_KH left(dost_odb,A_NRLTH)
#define D_KH1 if(val(dost_odb)="0",dost_odb,subs(dost_odb,A_NAZBEG))
#endif
#ifdef A_SUBDOK
#define D_SUBDOK +sub_dok
#else
#define D_SUBDOK
#endif
#ifdef A_ANKER
request asort
#endif
#ifdef A_NVAT
#define D_NVAT "D_VAT"
#else
#define D_NVAT "WART_VAT"
//#define d2biN(x) ((x)/100)
#define field2biN(x,y) fieldput(x,(y)/100)
#define bin2D(x) (100*(x))
#endif

field   data,smb_dow,nr_dowodu,pozycja,nr_zlec,ilosc,index,wart_vat,;
      data_dost,dost_odb,kontrahent,nr_faktury,nr_mag,kto_pisal,jm,nazwa,stan,cena,;
      czekiem,przelewem,uwagi,nr_rys,nr_czeku,proc_vat,ident,numer_kol,adres,;
      rodz_opak,gram,termin_p,przel,jm_opcja,nr_spec,transport,sub_dok,wart_ewid,;
      info,d_wartosc,wart_par,vat_par

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

#ifdef A_HPDF
  #define D_HWPRN A_HPDF
#endif
#ifdef A_WIN_PRN
  #define D_HWPRN A_WIN_PRN
#endif
#ifdef D_HWPRN
#command ?  [<explist,...>]         => WQ( <explist> )
#command ?? [<explist,...>]         => WQQ( <explist> )
#endif

#ifdef A_DRUKCOMP

#define EVLINE(buf,lbl,lc) while j>0 .and. j<=lc .and. (valtype(buf[j])#"C" .or. buf[j]<>lbl);
   ;self:=evline(buf,j++,@x);
   ;IF self<>NIL;
     ;IF self[3]<>NIL;
        ;x:=Self[3];
        ;PRIVATE &x;
     ;END;
     ;x:=&(self[1]);
   ;END;
;END
#define dok_p_r dok_def[1]
#define dok_zew dok_def[2]
#define dok_ew  dok_def[8]
#define dok_df  dok_def[A_DF]
#define dok_wal dok_def[A_WALUTA]
#ifdef A_LPTN
#define D_LPT ,dok_def[A_LPTN]
#else
#define D_LPT
#endif


memvar dok_kop
#ifdef A_IZ
field ilosc_f
#else
#define ilosc_f ilosc
#endif

#ifdef A_WA
field wartosc
#else
#ifdef A_FA
field wartosc
#endif
#endif

memvar i,j,k,s,p,r,w,v,wt,vt,ce,il,il_f,pv,nz,operator,dok_par,changed,mag_poz,dok_def,firma_n,;
       dokumenty,firma_a,fakkorflag,fakkormem,u,ut,gt,g,pm,was,defa,ilj,dok_naz,;
       mag_biez,magazyny,df,df_ver,AUX,numerp,datap,stawki,stawkizby,komunikat,self,;
       stary_rok,wabuf,buf,linecount,l,landscape,p_rownl,p_coln,oprn,na

procedure wydruk_dok(n,dok)
local mes,jb,x,y,z,m,o,srbuf
static apcomp:={}
private dok_kop:=n,linecount
private a,b,c,d,e,f,g,h,i,j,k,self,buf,l
private s,p,u,ut,pm,il,nz,il_f,na
#ifdef A_FA
private pv,ce,w,v,wt,vt,fakkorflag,fakkormem,wabuf
/*
#ifdef A_DFP
    private was:={{"22",0,0},{" 7",0,0},{" 0",0,0}}
#else
*/
    private was:={}
//#endif
#endif
#ifdef A_JMTOT
private ilj
#endif
#ifdef A_GRAM
private gt
#endif

  IF dok_kop=NIL
    dok_kop=1
    tone(262,2)
    mes:=message("Wydruk dokumentu;IloòÜ kopii:")
    x:=getnew(mes[1]+2,mes[2]+15,{|x|if(x=NIL,str(dok_kop,1),dok_kop:=val(x))},'dok_kop','#')
    GETREADER(x)
    if x:exitstate=K_ESC .or. dok_kop=0
    // if 2#alarm("CZY DRUKOWAè",{"NIE","TAK"})
      message(mes)
      return
    endif
    @ mes[1]+2,mes[2]+2 SAY "Prosz© czekaÜ"
  ELSE
  mes:=message("Prosz© czekaÜ;TRWA WYDRUK.")
  ENDIF

  begin sequence

  select main
#ifndef STANY
        SET RELATION TO INDEX INTO INDX_MAT
#else
        SET RELATION TO NR_MAG+INDEX INTO INDX_MAT
#endif
        SET ORDER TO "MAIN_NRK"

  select dM
#ifdef A_LAN
    go recno()
    LOCK recno()
#endif

#ifdef A_MM
  if empty(mag_biez)
     mag_biez:=left(magazyny[max(1,mag_poz)],2)
  endif
#else
    private mag_biez:=nr_mag
#endif
    private mag_poz:=max(1,ascan(magazyny,mag_biez))
    private r:=MAX(1,ascan(dokumenty[MAG_POZ],smb_dow D_SUBDOK))
    private dok_def:=dok_par[mag_poz,r]
    private dok_naz:=dokumenty[mag_poz,r]
#ifdef A_DF
#ifdef A_DFP
#ifdef A_CENVAT
  #define D_DF wartosc
#else
  #define D_DF (wartosc+wart_vat)
#endif
    private df:=dok_df.and.kto_pisal=chr(255)+chr(0),df_ver:=-1,aux,komunikat//,numerp,datap
#ifdef A_WP
    IF wart_par#0.and.(round(D_DF-wart_par,2)#0.or.round(vat_par-wart_vat,2)#0)
        alarm("WARTOóè WYDRUKOWANEGO PARAGONU WYNOSI:;"+ltrim(tran(wart_par,"@E"))+" Zù, w tym "+ltrim(tran(vat_par,"@E"))+" podatku VAT.;NIE MOΩNA DRUKOWAè RACHUNKU O WARTOóCI INNEJ NIΩ PARAGON.")
        df:=.f.
        break
    endif
#endif
#else
    private df:=.f.,aux,komunikat
#endif
#endif

        SELECT firmy
       set order to "FIRM_NUM"

       dbSEEK(dM->(D_KH),.f.)

    select dm

    l:=if(empty(dok),dok_def[7],dok)

    if valtype(l)$"MC"
     if len(l)<=12
        y:=UpP(l)
        if !"."$y
           y+=".ppr"
        endif
        x:=ascan(apcomp,{|x|x[1]==y})
        if x#0
          l:=apcomp[x,2]
        else
          x:=findfile(y)
          if !empty(x)
             l:=getlines(memoread(x))
             aadd(apcomp,{y,l})
          else
             l:=getlines(l)
          endif
        endif
     else
        l:=getlines(l)
     endif
     if empty(dok)
       dok_def[7]:=l
     endif
    endif

    buf:=dok:=l

    If empty(l)
       break
    endif

#ifdef D_HWPRN
    oprn:=D_HWPRN
#endif

    linecount:=len(dok)

    pm:=if(dok_p_r="P",1,-1)

    j:=1

    jb:=ascan(l,{|x|valtype(x)='C'.and.x+' '='HEADER '})


    while j>0 .and. (j<jb .or. j<=linecount .and. (x:=valtype(dok[j]))$'AC' .and. if(x="C" ,dok[j],dok[j,2])='PRIVATE')
      self:=evline(dok,j++,@x)
      IF self==NIL
      ELSE
       IF self[3]<>NIL
         x:=Self[3]
         PRIVATE &x
       ENDIF
       x:=&(self[1])
      ENDIF
    ENDDO

    if dok_kop=0 .or. j=0
       break
    endif

    IF j=jb
       ++j
    ENDIF

    set console off
    print(D_LPT)
    setprc(0,0)

#ifdef A_DFP
    changed:=kto_pisal=chr(255)
    if df
       aux:=NIL
       do while aux=NIL
          x:=getenv("MDSCOM")
          if ""=x
  #ifdef __PLATFORM__UNIX
             x:="/dev/ttyS0"
  #else
             x:="COM1"
  #endif
          endif
          aux:=rsopen(x)
          if aux<=0
             aux:=NIL
             IF 1=alarm("NIE POTRAFI® SI® PODù§CZYè DO DRUKARKI PARAGON‡W!;CZY PR‡BOWAè JESZCZE RAZ?",{"TAK","NIE"})
                loop
             ENDIF
             df:=.f.
             exit
          endif
          exit
       enddo
       if df
 #ifdef A_POSNET
         x:='\'
         df:=rswrite(aux,chr(24))=1 .and. dfprint('0$e',' ') .and. dfprint('#s',@x)
         if df
            k:=getlines(x,'/')
            was:=array((len(k)-5)/2)
            for x:=1 to len(was)
              was[x]:={str(val(k[x+1]),2),0,0}
            next
         else
 #else
         k:=array(7)
         afill(k,25500)
         x:=7
         df:=rs_stawki(@x,k)=0
         if df
           df_ver:=x
           was:=array(ascan(k,{|x|x>=10000},1,x)-1)
           aeval(k,{|x,i|was[i]:={str(x/100,2),0,0}},1,len(was))
         else
 #endif
           alarm('Paragon nie moæe byÜ wydrukowany!',,,3)
           df:=.f.
           rsclose(aux)
           aux:=NIL
         ENDIF
       endif
    ELSEIF aux#NIL
       x:=getenv("MDSCOM")
       if ""=x
  #ifdef __PLATFORM__UNIX
          x:="/dev/ttyS0"
  #else
          x:="COM1"
  #endif
       endif
       aux:=rsopen(x)
       IF AUX<=0
          rsclose(aux)
          aux:=NIL
       ELSE
 #ifdef A_POSNET
          rswrite(aux,chr(27)+'P2$d'+chr(12)+'RAZEM:'+strpic(D_DF-czekiem-przelewem,10,2,"@E ",.f.)+chr(27)+'\')
 #else
          dfprint(chr(27)+"R"+str(D_DF-czekiem-przelewem,9,2)+chr(10)+chr(27)+'S',chr(6))
 #endif
          rsclose(aux)
          aux:=NIL
       ENDIF
    endif
#endif

    jb:=j
    for i:=dok_kop to 1 step -1
      main->(dbSEEK(dM->(KEY_DOK+NR_DOWODU),.f.))
      message(1)
#ifdef A_DEMO
#ifdef A_FA
    if dok_p_r="F"
       ?? padl(left(firma_a,1+at(", ",firma_a))+dtoc(data),80)+chr(13)
  else
#endif
       ?? padr(firma_n,51),padl(cdow(dM->data),12),"dnia",dM->data
       ?
#ifdef A_FA
    endif
#endif
#endif
    j:=jb

    EVLINE(dok,"POZYCJE",linecount)
    l:=j+1
#ifdef A_DF
    IF dok_df
     if kto_pisal=chr(255)+chr(0)
       changed:=.t.
       kto_pisal:=chr(0)+operator
     elseif kto_pisal=chr(255)
       changed:=.t.
       kto_pisal:=chr(1)+operator
     endif
    ELSEIF KTO_PISAL=CHR(255)
#else
    IF KTO_PISAL=CHR(255)
#endif
          changed:=.t.
          KTO_PISAL:=OPERATOR
    ENDIF

      message(1)

#ifdef A_DFP
#ifdef A_POSNET
      df := df .and. dfprint('0$h')
#else
      df := df .and. dfprint(chr(27)+" "+l2bin(ROUND(D_DF*100,0)),chr(6))
#endif
 #undef D_DF
#endif

#ifdef A_FA
    do while .t.
#endif
    ce:=il:=u:=ut:=v:=w:=wt:=vt:=0
    nz:=''
    p:=D_LP0
#ifdef A_GRAM
    g:=gt:=0
#endif
#ifdef A_JMTOT
    ilj:={}
#endif
#ifdef A_FA
    pv:=''
    aeval(was,{|x,i|was[i,2]:=was[i,3]:=0})
    if !empty(fakkorflag)
#ifdef A_WB
       ? "Po korekcie:"
#else
       ? "Korekta:"
#endif
//       ?
       IF fakkorflag[4]#NIL
            stary_rok:=srbuf
            reuse()
            SELECT DM
            go fakkorflag[1]
            LOCK
       ELSE
            go fakkorflag[1]
       ENDIF
       main->(dbgoto(fakkorflag[2]))
       firmy->(dbgoto(fakkorflag[5]))
#ifdef A_DFP
       df:=fakkorflag[3]
#endif
       fakkorflag:=NIL

#ifdef A_WBX
    elseif fakkormem<>NIL
       ? "Korekta:"
       fakkormem:=NIL
       main->(dbSEEK(dM->(KEY_DOK+NR_DOWODU),.f.))
#endif
    elseif subs(dok_naz,2,1)="K"
#ifdef A_DFP
       fakkorflag:={recno(),main->(recno()),df,,firmy->(recno())}
#else
       fakkorflag:={recno(),main->(recno()),,,firmy->(recno())}
#endif
//#ifdef A_WB
#ifdef A_WB
       fakkormem:={}
#endif
       srbuf:=NIL
#ifndef A_KORALL
       wabuf:={}
       select MAIN
       DO while KEY_DOK+nr_dowodu=dM->(KEY_DOK+NR_DOWODU)
          aadd(wabuf,index)
          skip
       ENDDO
       go fakkorflag[2]
       SELECT DM
#endif
       if !empty(data_dost).and.year(data_dost)<year(data)
          srbuf:=stary_rok
          begin sequence

          stary_rok:=data_dost
          x:=left(dtos(data_dost),4)
          fakkorflag[4]:=x
          y:=pad(nr_faktury,D_MM)
          reuse()
          SELECT DM
          if dbseek(y)
            select MAIN
            dbseek(y,.f.)
            ? "Przed korekt•:"
//            ?
#ifdef A_DFP
            df:=.f.
#endif
          else
            break
          endif
          recover
            stary_rok:=srbuf
            reuse()
            SELECT FIRMY
            go fakkorflag[5]
            SELECT MAIN
            go fakkorflag[2]
            SELECT DM
            go fakkorflag[1]
            LOCK
            fakkorflag:=NIL
#ifdef A_WB
            fakkormem:=NIL
#endif
          end sequence
       else
       if dbSEEK(pad(nr_faktury,D_MM),.f.)
          ? "Przed korekt•:"
//          ?
          main->(dbSEEK(dM->(KEY_DOK+NR_DOWODU),.f.))
#ifdef A_DFP
          df:=.f.
#endif
       else
          go fakkorflag[1]
          fakkorflag:=NIL
#ifdef A_WB
          fakkormem:=NIL
#endif
       endif
       endif
    endif
#endif
      select main
      s:=NIL
      z:=''
      DO while KEY_DOK+nr_dowodu=dM->(KEY_DOK+NR_DOWODU)
#ifdef A_FA
         if !empty( fakkorflag)
#ifndef A_KORALL
           k:=ascan(wabuf,index)
           if k=0
            do while j<=linecount .and. !(valtype(dok[j])="C" .and. dok[j]="SUMA")
              j++
            enddo
            skip
            LOOP
           endif
#endif
#ifdef A_WB
            aadd(fakkormem,{index,pm*ilosc_f,cena,proc_vat,if(srbuf=NIL,recno(),0),pozycja,nr_zlec})
         elseif fakkormem#NIL
*
          x:=getlines(trim(subs(dm->nr_faktury,D_MM+2)),',')
          y:=ascan(fakkormem,{|x|index=pad(x[1],len(index)).and.round(x[3]-cena,2)=0.and.x[4]=proc_vat})
          if len(x)>0 .and. y>0
             for k:=1 to len(x)
                y:=ascan(fakkormem,{|a|a[6]=D_LPPUT(val(x[k])) .and. index=pad(a[1],len(index)).and.round(a[3]-cena,2)=0.and.a[4]=proc_vat })
                if y<>0
                   exit
                endif
             next k
          endif
*
#ifdef A_KORALL
          if y=0 .and. z#index
             y:=len(fakkormem)+1
          endif
          if y>1
            x:=recno()

            aeval(fakkormem,{|x|wbline(x,dok)},1,y-1)
            if y>len(fakkormem)
               fakkormem:={}
            else
               acopy(fakkormem,fakkormem,y)
               asize(fakkormem,len(fakkormem)-y+1)
            endif
            go x
            y:=min(1,len(fakkormem))
          endif
#endif
#endif
         endif
#endif
#ifndef STANY
        indx_mat->(dbseek(main->INDEX,.f.))
#else
        indx_mat->(dbseek(main->NR_MAG+main->INDEX,.f.))
#endif
         message(1)
         j:=l
         s:=i_lam(dM->data)
         il:=pm*ilosc
         il_f:=pm*ilosc_f
         nz:=nr_zlec
#ifndef A_WA
   #define WartosC (s)->cenA*il*pm
#endif
         u:=pm*WartosC
         ut+=u
#undef WartosC


#ifdef A_FA
#ifdef A_WB
       z:=index
       if empty(fakkorflag) .and. !empty(fakkormem) .and. y#0
          il_f:=il_f+fakkormem[y,2]
          adel(fakkormem,y)
          asize(fakkormem,len(fakkormem)-1)
          nz:=space(len(nz))
*
          if il_f=0
             do while j<=linecount .and. !(valtype(dok[j])="C" .and. dok[j]="SUMA")
                j++
             enddo
             skip
             LOOP
          endif
*
       endif
#endif
   pv:=proc_vat
   ce:=cena
   if dok_p_r="F"
      w:=WGR(il_f,ce,val(pv),dok_df)
      v:=ILEVATGR(il_f,ce,val(pv),dok_df)
   else
      w:=ROUND(pm*ce*100,0)   // bez vat
      v:=VATPZGR(pm*ce,val(pv))
   endif
      k:=ascan(was,{|x|x[1]=pv})
      if k=0
       aadd(was,{pv,0,0})
       k:=len(was)
      endif
      was[k,2]+=w
      was[k,3]+=v
      wt+=w
      vt+=v

   if dok_p_r="F"
      w:=W(il_f,ce,val(pv),dok_df)
      v:=ILEVAT(il_f,ce,val(pv),dok_df)
   else
      w:=pm*ce   // bez vat
      v:=VATPZ(w,val(pv))
   endif
#ifdef A_DFP
       komunikat:="0"
#endif
#endif

#ifdef A_GRAM
      gt+=g:=il_f*(s)->gram/1000
#endif
#ifdef A_JMTOT
#ifdef A_IZ
#define il_f if(IL_F=0,il,IL_F)
#endif
#ifdef A_JMALTTOT
        y:=ascan(ilj,{|x|x[1]=(s)->jm_opcja})
        if y=0
           aadd(ilj,{(s)->jm_opcja,A_JMALTTOT(il_f,nz,s,x)})
        else
           ilj[y,2]+=A_JMALTTOT(il_f,nz,s,x)
        endif
#endif
      y:=ascan(ilj,{|x|x[1]=(s)->jm})
      if y=0
       aadd(ilj,{(s)->jm,il_f})
      else
       ilj[y,2]+=il_f
      endif
#ifdef A_IZ
#undef il_f
#endif
#endif
       p:=D_LPPUT(D_LPVAL(p)+1)
       select (s)
       na:=nazwa

       EVLINE(dok,"SUMA",linecount)

       select main

#ifdef A_DFP
#ifdef A_CENVAT
 #define D_DF w
#else
 #define D_DF (w+v)
#endif
      if df
        if was[k,1]=='zw'
           k:=0
        endif
 #ifdef A_POSNET
        df := dfprint(D_LPSTR(p)+'$l'+trim(left(na,40))+chr(13)+str(il_f,11,3)+chr(13)+chr(k+64)+'/'+str(WDFGR(1,ce,val(pv),.t.)/100,10,2)+'/'+str(D_DF,10,2)+'/')
 #else
        x:=3
        y:=ROUND(1000*il_f,0)
        do while x>0 .and. y%10=0
           --x
           y/=10
        enddo
        if k=0
           k:=5
        elseif k>4
           ++k
        endif
        df := df .and. dfprint(chr(27)+chr(if(df_ver=7,5,6))+IF(y<0,"-"," ")+PAD(if(len(trim(na))<10,PAD(trim(na),10,'.'),na),if(df_ver=7,40,28))+komunikat+l2bin(abs(y))+chr(x)+' '+left((s)->jm,3)+l2bin(WzVATGR(1,ce,val(pv),.t.)));
		 .and. dfprint(chr(27)+chr(k+64)+l2bin(abs(ROUND(D_DF*100,0))))
/*
        if df_ver=7
          x  := i2bin(6)
          df := df .and. dfprint(chr(27)+chr(0x50),@x) .and. x == i2bin(6)
        else
          x  := chr(0)
          df := df .and. dfprint(chr(27)+chr(0x95),@x) .and. asc(x) %32 < 16
        endif
*/
 #endif
      endif
 #undef D_DF
#endif
     skip
    enddo
#ifdef A_WB
    if !empty(fakkorflag)
       wabuf:={ut,aclone(was),wt,vt}
    elseif fakkormem#NIL
       y:=recno()
       aeval(fakkormem,{|x|wbline(x,dok)})
       go y
       y:={ut,aclone(was),wt,vt}
       aeval(wabuf[2],{|x,i|was[i,2]-=wabuf[2,i,2],was[i,3]-=wabuf[2,i,3]})
       wt-=wabuf[3]
       vt-=wabuf[4]
       wabuf:={ut,aclone(was),wt,vt}
       was:=y[2]
       wt:=y[3]
       vt:=y[4]
       ++j
    message(1)
    select dm
#ifdef A_NVAT
    v:=vt/100
    w:=wt/100
    vt:=wt:=0
#ifdef A_DF
#ifdef A_CENVAT
      if !dok_df
         aeval(was,{|x,w|w:=round(x[2]-x[3],0),wt+=w,x[3]:=w*val(x[1])/100,x[2]:=w+ROUND(x[3],0),vt+=ROUND(x[3],0)})
         wt+=vt
#else
      if  dok_df
         aeval(was,{|x,w|w:=round(x[2]+x[3],0),wt+=w,x[3]:=w*val(x[1])/(100+val(x[1])),x[2]:=w-ROUND(x[3],0),vt+=ROUND(x[3],0)})
         wt-=vt
#endif
      else
#endif
#ifdef A_CENVAT
         aeval(was,{|x|x[2]:=round(x[2],0),wt+=x[2],x[3]:=x[2]*val(x[1])/(100+val(x[1])),vt+=ROUND(x[3],0)})
#else
         aeval(was,{|x|x[2]:=round(x[2],0),wt+=x[2],x[3]:=x[2]*val(x[1])/100,vt+=ROUND(x[3],0)})
#endif
#ifdef A_DF
      endif
#endif
#endif
      vt/=100
      wt/=100
#ifdef A_NVAT
      aeval(was,{|x|x[2]/=100,x[3]:=round(x[3],0)/100})
#else
      aeval(was,{|x|x[2]/=100,x[3]/=100})
#endif

      EVLINE(dok,"STOPKA",linecount)
#ifdef A_WBX
      loop
#else
      s:=NIL
      ? "Korekta:"
      ut:=wabuf[1]
      was:=wabuf[2]
      wt:=wabuf[3]
      vt:=wabuf[4]
      select main
      skip -1
#endif
    endif
#endif
    if s=NIL //pusty przebieg !
       j:=l
       do while j<=linecount .and. !(valtype(dok[j])="C" .and. dok[j]="SUMA")
          j++
       enddo
    else
       skip -1
    endif
      ++j
    message(1)
    select dm

#ifdef A_FA
#ifdef A_NVAT
    v:=vt/100
    w:=wt/100
    vt:=wt:=0
    if dok_p_r="F"
#ifdef A_DF
#ifdef A_CENVAT
      if !dok_df
         aeval(was,{|x,w|w:=round(x[2]-x[3],0),wt+=w,x[3]:=w*val(x[1])/100,x[2]:=w+ROUND(x[3],0),vt+=ROUND(x[3],0)})
         wt+=vt
#else
      if  dok_df
         aeval(was,{|x,w|w:=round(x[2]+x[3],0),wt+=w,x[3]:=w*val(x[1])/(100+val(x[1])),x[2]:=w-ROUND(x[3],0),vt+=ROUND(x[3],0)})
         wt-=vt
#endif
      else
#endif
#ifdef A_CENVAT
         aeval(was,{|x|x[2]:=round(x[2],0),wt+=x[2],x[3]:=x[2]*val(x[1])/(100+val(x[1])),vt+=ROUND(x[3],0)})
#else
         aeval(was,{|x|x[2]:=round(x[2],0),wt+=x[2],x[3]:=x[2]*val(x[1])/100,vt+=ROUND(x[3],0)})
#endif
#ifdef A_DF
      endif
#endif
      else
#ifdef A_PZBRUT
         aeval(was,{|x|x[2]:=round(x[2],0),wt+=x[2],x[3]:=x[2]*val(x[1])/(100+val(x[1])),vt+=ROUND(x[3],0)})
#else
         aeval(was,{|x|x[2]:=round(x[2],0),wt+=x[2],x[3]:=x[2]*val(x[1])/100,vt+=ROUND(x[3],0)})
#endif
      endif
#endif
      vt/=100
      wt/=100
      if fakkorflag=NIL
         y:=.f.
#ifdef A_A
         for n:=1 to len(stawki)
             z:=stawki[n]
             if empty(z)
               loop
             endif
             x:=ascan(was,{|x|x[1]==z})
             o:=fieldget(fieldpos('wart_net'+ltrim(z)))
             if o=NIL
                o:=0
             endif
             if x<>0
               z:=was[x,2]
             #ifdef A_CENVAT
               if dok_p_r="F"
                 z-=was[x,3]
               endif
             #endif
             #ifdef A_PZBRUT
               if dok_p_r<>"F"
                 z-=was[x,3]
               endif
             #endif
             endif
             if y:=if(x=0,o<>0,round(o-z/100,2)<>0)
                exit
             endif
         next n
#endif
      if y.or.dok_zew$"UV" .and. ROUND(warT_vaT-vt,A_ZAOKR)#0
         if i=dok_kop .and. (tone(130,3),if(changed,1=alarm("NIEZGODNA WARTOóè PODATKU;CZY DOKONAè KOREKTY ?",{"TAK","NIE"},2,2),NIL=alarm("NIEZGODNA WARTOóè PODATKU;NIE DOKONANO KOREKTY !!!")))
             //changed:=.t.
#ifdef A_A
             for n:=1 to len(stawki)
               z:=stawki[n]
             if empty(z)
               loop
             endif
               x:=ascan(was,{|x|x[1]==z})
               if x<>0
                 if dok_p_r="F"
               #ifdef A_CENVAT
                    z:=was[x,2]-was[x,3]
               #else
                    z:=was[x,2]
               #endif
                 else
               #ifdef A_PZBRUT
                    z:=was[x,2]-was[x,3]
               #else
                    z:=was[x,2]
               #endif
                 endif
               endif
               fieldput(fieldpos('wart_net'+ltrim(stawki[n])),if(x=0,0,z/100))

             next n
#endif
             wart_vat:=vt
             for n:=1 to len(stawki)
                z:=stawki[n]
                if val(z)#0
                   x:=ascan(was,{|x|x[1]=z})
                   if x#0
                      x:=was[x,3]
                   endif
                   field2biN(fieldpos(D_NVAT+ltrim(z)),x)
                endif
             next n
         else
             vt:=wart_vat
             for k:=1 to len(stawki)
                if val(stawki[k])#0 .and. (n:=fieldpos(D_NVAT+ltrim(stawki[k])))#0
                   x:=ascan(was,{|x|x[1]=stawki[k]})
                   if x=0
                      if bin2D(fieldget(n))#0
                         aadd(was,{stawki[k],0,bin2D(fieldget(n))})
                      endif
                   else
                      was[x,3]:=bin2D(fieldget(n))
                   endif
                endif
             next k
         endif
      endif
      if dok_ew#"E" .and. ROUND(wartosc-wt,A_ZAOKR)#0
      if i=dok_kop .and.changed
         WARTOSC:=wt
         #ifdef A_DF
          field2bin('d_wartosc',w)
         #endif
         //changed:=.t.
         alarm("WARTOóè SUMY POZYCJI NIEZGODNA Z INFORMACJ§ W NAGù‡WKU;DOKONANO KOREKTY",,,3)
      else
         alarm("NIEZGODNA WARTOóè SUMY POZYCJI;NIE DOKONANO KOREKTY !!!",,,3)
         wt:=WARTOSC
      endif
      endif
#endif
      if POZYCJA#MAIN->pozycja
        changed:=.t.
        alarm("ILOóè POZYCJI NIEZGODNA Z INFORMACJ§ W NAGù‡WKU;DOKONANO KOREKTY",,,3)
        pozycja:=MAIN->pozycja
      ENDIF
#ifdef A_WE
      if ROUND(warT_ewiD-pm*ut,A_ZAOKR)#0
        changed:=.t.
        alarm("WARTOóè EWIDENCYJNA SUMY POZYCJI NIEZGODNA Z INFORMACJ§ W NAGù‡WKU;DOKONANO KOREKTY",,,3)
        warT_ewiD:=pm*ut
      ENDIF
#endif
#ifdef A_FA
      endif
#ifdef A_NVAT
      aeval(was,{|x|x[2]/=100,x[3]:=round(x[3],0)/100})
#else
      aeval(was,{|x|x[2]/=100,x[3]/=100})
#endif
#endif
      EVLINE(dok,"STOPKA",linecount)
      ++j
#ifdef A_FA
      if !empty(fakKorFlag)
          loop
      endif
      exit
      enddo
#endif

     message(1)
#ifdef A_DFP
 #ifndef A_POSNET
    df := df .and. dfprint(chr(27)+chr(7),"") // nie pr¢buje czytaÜ
 #endif
#endif

    EVLINE(dok,"RETURN",linecount)
    if prow()>0
       specout(chr(13)+chr(12))
       setprc(0,0)
    endif

#ifdef A_DFP
      if aux#NIL
         if df
            tone(164.8,1)
            tone(164.8,1)
 #ifdef A_LAN
            if SET(_SET_PRINTER)
               set print off
  #ifdef A_PRINT
               x:=set(_SET_PRINTFILE,'')
               if ! set(_SET_PRINTFILE)==x .and. File(x)
                 A_PRINT(x)
               endif
  #else
               set printer to
  #endif
               set printer to
               if i>1
                  print(0)
               endif
            endif
 #endif
            @ mes[1],mes[2]+2 SAY 'Kwota Wpàaty:'
            @ mes[3],mes[2]+2 SAY '[Esc] - ANULUJ'
            @ mes[3]-1,mes[2]+2 SAY '   (reszta)   '
            x:=0
            k:=getnew(mes[1]+1,mes[2]+2,{|z|if(pcount()=0,x,x:=z)},"x","@E ## ### ###.##")
 #ifdef A_CENVAT
            k:postblock:={||setpos(mes[3]-1,mes[2]+2),dispout(if(x=0,'              ',tran(x-wt,"@E ## ### ###.##"))),.t.}
 #else
            k:postblock:={||setpos(mes[3]-1,mes[2]+2),dispout(if(x=0,'              ',tran(x-wt-vt,"@E ## ### ###.##"))),.t.}
 #endif
            readmodal({k})
            if readkey() = K_ESC
               df:=.f.
               i:=1
            endif
 #ifdef A_POSNET
         endif

  #ifdef A_CENVAT
         if df .and. (k:=' ',dfprint('1;0;1;0$e1'+left(operator,1)+subs(operator,1+at(' ',operator),1)+chr(13)+"#"+nr_mag+smb_dow+nr_dowodu+chr(13)+str(x,10,2)+'/'+str(wt,10,2)+'/',@k).and.asc(k)%8=5)
  #else
         if df .and. (k:=' ',dfprint('1;0;1;0$e1'+left(operator,1)+subs(operator,1+at(' ',operator),1)+chr(13)+"#"+nr_mag+smb_dow+nr_dowodu+chr(13)+str(x,10,2)+'/'+str(wt+vt,10,2)+'/',@k).and.asc(k)%8=5)
  #endif
 #else
            if x#0
              df:=DF .AND. dfprint(chr(27)+chr(9)+'0'+strpic(x,26,2,'@E ',.f.)+chr(10))
  #ifdef A_CENVAT
              df:=DF .AND. dfprint(chr(27)+chr(9)+'1'+strpic(x-wt,29,2,'@E ',.f.)+chr(10))
  #else
              df:=DF .AND. dfprint(chr(27)+chr(9)+'1'+strpic(x-wt-vt,29,2,'@E ',.f.)+chr(10))
  #endif
            endif
         endif
         if df .and. dfprint(chr(27)+"$",chr(6))
 #endif

            kto_pisal:=operator
 #ifdef A_WP
  #ifdef A_CENVAT
   #define D_DF wt
  #else
   #define D_DF (wt+vt)
  #endif
            wart_par:=D_DF
            vat_par:=vt
  #undef D_DF
 #endif
            changed:=.t.
         else
 #ifdef A_POSNET
            rswrite(aux,chr(24))
            dfprint('0$e')
            alarm("WYDRUK PARAGONU ZOSTAù ANULOWANY!",,,3)
 #else
            if !df
               dfprint(chr(27)+"#",chr(6),"PARAGON NIE ZOSTAù WYDRUKOWANY PRAWIDùOWO!;") //uniewaænienie paragonu i test przy okazji
            endif
 #endif
         endif
         df:=.f.
         rsclose(aux)
         aux:=NIL
      endif
#endif DFP
  next i
  END SEQUENCE
#ifdef D_HWPRN
if valtype(oprn)='O'
   oprn:Destroy()
   oprn:=NIL
endif
#endif
  UNLOCK IN DM
  UNLOCK IN MAIN
  UNLOCK IN STANY
  message(mes)
  SET PRINT OFF
#ifdef A_PRINT
               x:=set(_SET_PRINTFILE,'')
               if ! set(_SET_PRINTFILE)==x .and. File(x)
               A_PRINT(x)
               endif
#else
               set printer to
#endif
#ifdef A_DFP
  if aux#NIL
#ifdef A_POSNET
     rswrite(aux,chr(24))
     dfprint('0$e')
     alarm("WYDRUK PARAGONU ZOSTAù ANULOWANY!",,,3)
#else
     dfprint(chr(27)+"#",chr(6),"PARAGON NIE ZOSTAù WYDRUKOWANY PRAWIDùOWO!;") //uniewaænienie paragonu i test przy okazji
#endif
     rsclose(aux)
  endif
#endif
  SET CONSOLE ON
return
****************************
#ifdef A_WB
procedure wbline(fkm,dok)
//{index,pm*ilosc_f,cena,proc_vat,if(srbuf=NIL,recno(),0),pozycja,nr_zlec}
local x,y
private j:=l
         if fkm[5]>0
         go fkm[5]
         endif

#ifndef STANY
        indx_mat->(dbseek(fkm[1],.f.))
#else
        indx_mat->(dbseek(mag_biez+fkm[1],.f.))
#endif

         message(1)
         p:=D_LPPUT(D_LPVAL(p)+1)
         s:=i_lam(dM->data)
         u:=il:=0
         il_f:=fkm[2]
         nz:=fkm[7]
         pv:=fkm[4]
         ce:=fkm[3]
   if dok_p_r="F"
      w:=WGR(il_f,ce,val(pv),dok_df)
      v:=ILEVATGR(il_f,ce,val(pv),dok_df)
   else
      w:=ROUND(pm*ce*100,0)   // bez vat
      v:=VATPZGR(pm*ce,val(pv))
   endif
      k:=ascan(was,{|x|x[1]=pv})
      if k=0
       aadd(was,{pv,0,0})
       k:=len(was)
      endif
      was[k,2]+=w
      was[k,3]+=v
      wt+=w
      vt+=v
#ifdef A_GRAM
      gt+=g:=il_f*(s)->gram/1000
#endif
#ifdef A_JMTOT
#ifdef A_JMALTTOT
        y:=ascan(ilj,{|x|x[1]=(s)->jm_opcja})
        if y=0
           aadd(ilj,{(s)->jm_opcja,A_JMALTTOT(il_f,nz,s,x)})
        else
           ilj[y,2]+=A_JMALTTOT(il_f,nz,s,x)
        endif
#endif
      y:=ascan(ilj,{|x|x[1]=(s)->jm})
      if y=0
       aadd(ilj,{(s)->jm,il_f})
      else
       ilj[y,2]+=il_f
      endif
#endif
   if dok_p_r="F"
      w:=W(il_f,ce,val(pv),dok_df)
      v:=ILEVAT(il_f,ce,val(pv),dok_df)
   else
      w:=pm*cena   // bez vat
      v:=VATPZ(w,val(pv))
   endif
       select (s)

       EVLINE(dok,"SUMA",linecount)
       select main
return
#endif
*****************************

#ifdef A_DFP
 #ifdef A_POSNET
#include 'hbcom.ch'
function rswrite(n, c, l)
  local ret:=hb_comSend( n, c, l, D_TIMEOUT )
  hb_idlestate()
return ret
************************
function rsclose(n)
return hb_comClose( n )
************************
function rsopen(x)

  if empty(x)
    x:=getenv("MDSCOM")
      if ""=x
  #ifdef __PLATFORM__UNIX
         x:="/dev/ttyS0"
  #else
         x:="COM1"
  #endif
      endif
   endif
   hb_comSetDevice( 3, x )
   hb_comClose( 3 )
   IF hb_comOpen( 3 )
    IF hb_comInit( 3, A_DFP, 'N', 8, 1 ) .and. hb_comflowcontrol(3, @x, HB_COM_FLOW_XON) .and. hb_comflowchars(3,17,19)
         hb_idlestate()
         RETURN 3
      ELSE
         hb_comClose( 3 )
      ENDIF
   ENDIF

RETURN -1
*****************************
function rsread(n,x,y)
local c:=chr(0), i:=0
if valtype(x)<>'C' .or. !empty(y) .and. len(x)<y
   x:=space(y)
endif
if empty(y)
   y:=len(x)
endif
while hb_comRecv( n, @c, 1 , D_TIMEOUT ) = 1
   x:=stuff(@x,++i,1,c)
   if i=y
      exit
   endif
end

return i
***********************
static function chgmaz(a)
external HB_CODEPAGE_PLMAZ
return HB_TRANSLATE(a,,'PLMAZ')
************************
static function poscheck(a)
local i,b:=511
for i:=1 to len(a)
  b:= hb_bitxor(b,asc(subs(a,i,1)))
next
return subs(HB_NUMTOHEX(b),2)
************************
func dfprint(ft,b)
local ret:=.t.,mes,ndclose:=.f.,closeaux,c
memvar aux
if type('aux')='U'
   private aux
endif
if valtype(aux)<>'N'
   closeaux:=aux
   aux:=rsopen(closeaux)
   if aux<1
      aux:=closeaux
      return .f.
   endif
   ndclose:=.t.
endif
  if Len(ft)>0
  #ifdef PC852
     ft:=chgmaz(ft)
  #endif
     ret:=rswrite(aux,chr(27)+'P'+ft+poscheck(ft)+chr(27)+'\')=len(ft)+6
  endif
if ret .and. b#NIL
   if b='\'
     b:=''
     c:=chr(0)
     while hb_comRecv( aux, @c, 1 , D_TIMEOUT ) = 1
       b+=c
       if ret:=right(b,2)==chr(27)+'\'
          exit
       endif
     enddo
   else
     hb_comFlush( aux, 1 )
     rswrite(aux,chr(5))
     do while .t.
       b:=chr(0)
       if inkey()=27 .or. hb_comRecv(aux,@b,1, D_TIMEOUT)=1 .and. b>='`'.and. b<='o'
          exit
       endif
       if mes=NIL
          mes:=message("Czekam na odpowied´ drukarki;[Esc] - rezygnuj")
       endif
     enddo
     ret:=asc(b)%8>=4
   endif
   message(mes)
endif
if ndclose
   rsclose(aux)
   aux:=closeaux
endif
return ret
 #else
   #ifdef A_DFPDOS
   //__PLATFORM__WINDOWS

function rsopen(x)
  if empty(x)
    x:=getenv("MDSCOM")
      if ""=x
         x:="COM1"
      endif
   endif
return IF(RS_INIT(val(subs(x,4)),A_DFP, D_TIMEOUT /1000)=0,44,-44)

function rsclose()
return RS_DONE()=0

function rswrite(x,buf,n)

  if n=NIL .or. Len(buf)<n
    n:=Len(buf)
  endif
  if n>=2 .and. ! RS_SEND(asc(subs(buf,2,1)),,subs(buf,3,n-2))
    n:=0
  endif

RETURN n

func dfprint(rtr,ack,l)
local s:=' ',ret,disp,x,kod

     if rtr<>chr(27)
        return .t.
     endif

     if l=NIL
        l:=""
     endif

     kod:=asc(subs(rtr,2,1))
     rtr:=subs(rtr,3)
     ret:=.f.

     if ack=NIL
        ack:=''
     endif

     DO WHILE .T.
        disp:=""#l
        s:=space(len(ack))
        if RS_SEND(kod,@s,rtr)=0
           if ack=='' .or. s<>chr(21)
              ret:=.t.
           endif
           ack:=pad(s,len(ack))
           if !disp .and. ret
              exit
           endif
        endif
        disp:=.t.


     begin sequence
     s:=' '
     if RS_SEND(148,@s,'')<>0 .or. S>=chr(0x80)
        break
     endif
     S:=ASC(S)
     if s%128>=64
        l+=";BRAK WYóWIETLACZA KLIJENTA."
        disp:=.t.
     endif
     if s%64>=32
        l+=";NAST§PIùO ZABLOKOWANIE NAZWY TOWARU W PARAGONIE."
        rtr:=NIL
        disp:=.t.
     endif
     if s%32>=16
        l+=";SKASOWANY CMOS."
        rtr:=NIL
        disp:=.t.
     endif
     if s%16>=8
        l+=";ZALEGùY RAPORT DOBOWY."
        disp:=.t.
     endif
     if s%8>=4
        l+="W PAMI®CI FISKALNEJ DRUKARKI ZOSTAùO MAùO MIEJSCA."
        disp:=.t.
     endif
     if s%4>=2
        l+=";W PAMI®CI DRUKARKI ZNAJDUJE SI® DOKUMENT DO WYDRUKOWANIA."
     endif
     if s%2=1
        l+=";BRAK WOLNEGO MIEJSCA W BAZIE KONTRLONEJ NAZW I STAWEK."
        rtr:=NIL
        disp:=.t.
     endif
     s:=' '
     if RS_SEND(155,@s,'')<>0
        break
     endif
     S:=ASC(S)
     if s%8>=4
        l+=';MODUù FISKALNY W TRYBIE "TYLKO ODCZYT"!'
        rtr:=NIL
        disp:=.t.
     endif
     s:=' '
     if RS_SEND(149,@s,'')<>0 .or. S>='Ä'
        break
     endif
     S:=ASC(S)
     if s%128>=64
        l+=";WYDRUK PARAGONU ZATRZYMANY Z POWODU BRAKU PAPIERU."
        disp:=.t.
     endif
     if s%32>=16
        l+=";NAST§PIùO UNIEWAΩNIENIE PARAGONU."
        disp:=.t.
     endif
     if s%16>=8
        l+=";ZA NISKIE NAPI®CIE AKUMULATORA - POWIADOM SERWIS."
        disp:=.t.
     endif
     if s%8>=4
        l+=";AWARIA MECHANIZMU DRUKARKI."
        disp:=.t.
     endif
     if s%4>=2
        l+=";BRAK PAPIERU."
        disp:=.t.
     endif
     if s%2=1
        l+=";W BUFORZE DRUKOWANIA S§ ZNAKI DO WYDRUKOWANIA."
     endif

     recover
        l+=";BRAK KOMUNIKACJI."
        ret:=.f.
        rtr:=NIL
        s:=3
     end sequence
     IF !ret .AND. s%4<2
        s:=' '
        if "DO WYDR" $ l
           tone(130,3)
           if 1=alarm("Bù§D DRUKARKI PARAGON‡W!"+l+";CZY PR‡BOWAè JESZCZE RAZ?",{"TAK","NIE"})
              if RS_SEND(42,@s,'')<>0 .and. s=chr(6)
                 alarm("PONAWIAM WYDRUK!")
              endif
              l:=""
              loop
           endif
           exit
        endif
        if "RAPORT D" $ l
           tone(130,3)
           if 1=alarm("Bù§D DRUKARKI PARAGON‡W!"+l+";CZY PR‡BOWAè JESZCZE RAZ?",{"TAK","NIE"})
              if RS_SEND(37,@s,'')<>0 .and. s=chr(6)
                 alarm("DRUKUJ® ZALEGùY RAPORT DOBOWY!")
              endif
              l:=""
              loop
           endif
           exit
        endif
     ENDIF
     IF disp .or. !ret
        tone(130,3)
        if !RET .AND. !empty(rtr)
           if 1=alarm("Bù§D DRUKARKI PARAGON‡W!"+l+";CZY PR‡BOWAè JESZCZE RAZ?",{"TAK","NIE"})
              l:=""
              loop
           endif
        elseif !empty(l)
           alarm("DRUKARKA PARAGON‡W:"+l)
        endif
     ENDIF
     exit
     enddo

return RET

#pragma BEGINDUMP

#include "hbapi.h"

typedef unsigned char tBufferOfBytes[256];

#include "elzabdr.h"

HB_FUNC ( RS_INIT )
{
   hb_retnl( CommunicationInit( hb_parni(1), hb_parni(2), hb_parni(3) ) );
}

HB_FUNC ( RS_DONE )
{
   hb_retnl( CommunicationEnd() );
}

HB_FUNC ( RS_SEND )
{

  HB_SIZE bufl = hb_parclen( 2 );

  if (bufl<=256) {
    char * buf = hb_xgrab( bufl + 1 );
    hb_retnl( RSSequence( hb_parni(1), bufl, hb_parclen(3), buf, hb_parc( 3 ) ) );
    hb_storclen_buffer( buf, bufl, 2 );
  } else hb_retnl( -1 );

}

HB_FUNC ( RS_STAWKI )
{

long ile = 5, a , b , c , d , e , f , g ;

hb_retnl( ReadVAT ( & ile, & a, & b , & c , & d, & e, & f, & g ) );

hb_stornl(ile,1);
hb_storvnl(a,2,1);
hb_storvnl(b,2,2);
hb_storvnl(c,2,3);
hb_storvnl(d,2,4);
hb_storvnl(e,2,5);
hb_storvnl(f,2,6);
hb_storvnl(g,2,7);

}
#pragma ENDDUMP

   #else

#include 'hbcom.ch'
function rswrite(n, c, l)
  local ret
  ret:=hb_comSend( n, c, l, D_TIMEOUT )
  hb_idlestate()
return ret
************************
function rsclose(n)
return hb_comClose( n )
************************
function rsopen(x)

   if empty(x)
      x:=getenv("MDSCOM")
      if ""=x
  #ifdef __PLATFORM__UNIX
         x:="/dev/ttyS0"
  #else
         x:="COM1"
  #endif
      endif
   endif
   hb_comSetDevice( 3, x )
   hb_comClose( 3 )
   IF hb_comOpen( 3 )
      IF hb_comInit( 3 , A_DFP, 'E', 8, 1 ) .and. hb_comflowcontrol(3, @x, HB_COM_FLOW_ORTSCTS)
         hb_idlestate()
         RETURN 3
      ELSE
         hb_comClose( 3 )
      ENDIF
   ENDIF

RETURN -1
***********************
function rsread(n,x,y)
local c:=chr(0), i:=0
if valtype(x)<>'C' .or. !empty(y) .and. len(x)<y
   x:=space(y)
endif
if empty(y)
   y:=len(x)
endif
while hb_comRecv( n, @c, 1 , D_TIMEOUT ) = 1
   x:=stuff(@x,++i,1,c)
   if i=y
      exit
   endif
end

return i
***********************
func rs_stawki( l, stawki )
local s := space(14),i
   i:=chr(6)+chr(0)
   dfprint(chr(27)+chr(0xFF),@i)
   i:=asc(subs(i,2))
   if i=0x2c
     l := 7
     s:=chr(6)+space(l * 2)
     i:=dfprint(chr(27)+chr(0xd1),@s)
   elseif i=0x11
     l := 6
     s:=chr(6)+space(l * 2)
     i:=dfprint(chr(27)+chr(0xd0),@s)
   else
     l := 4
     s:=chr(6)+space(l)
     i:=dfprint(chr(27)+'p',@s)
   endif
   if i
     if l>4
       for i:=1 To l
         stawki[i]:=(asc(subs(s,2*i,1))*256+asc(subs(s,2*i+1,1)))
       next
     else
       for i:=1 to l
         stawki[i]:=asc(subs(s,i+1,1))*100
       next
     endif
     return 0
   endif
return -1
************************
func dfprint(rtr,ack,l)  //fp600
local s,ret,disp,x,ndclose:=.f.,lack
memvar aux,df_ver

     s:=type('df_ver')
     if s<>'N'
       df_ver:=-1
     endif
     
     s:=type('aux')
     if s<>'N' .or. aux<>3
        if rsopen(if(s='C',aux,))<>3
           return .f.
        endif
        ndclose:=.t.
     endif
     if valtype(ack)<>'C'
        ack:=''
     endif
     disp := valtype(l)='C'
     if empty(l)
        l:=''
     endif
     lack := ( ack = chr(6) )
     ret:=.f.
     x:=seconds()
     DO WHILE .T.
        if seconds()>x+D_TIMEOUT/1000
           exit
        endif
        s:=hb_comOutputCount( 3 )
        if s > 0
          HB_IDLESLEEP(10*s/A_DFP)
          LOOP
        endif
/***********
//#define HB_COM_TX_CTS               0x01
        s:=hb_comOutputState( 3 )
        IF HB_BITAND(s, HB_COM_TX_CTS)<>0
          HB_IDLESLEEP(.1)
          LOOP
        endif
************/
     if empty(rtr)
        ret :=  len(ack) = 0 .or. rsread( 3, @ack, len(ack) ) = len(ack)
     else
        ret:=.t.
        s:=''
        if len(ack)>0
           ret := rswrite( 3, rtr, 2 ) == 2
           s:=chr(0)
           ret := ret .and. ( hb_comRecv( 3, @s, 1, D_TIMEOUT ) == 1 ) .and. (!lack .or. asc(s) = 6)
           ack := stuff( ack ,1, 1, s)
           rtr := subs( rtr, 3 )
        endif
        if ret
           ret := ( rtr == '' ) .or. rswrite( 3, rtr ) == len( rtr )
           if (len(ack)>1)
             ack:=space(len(ack)-1)
/*
             if df_ver<7 .and. len(ack)>1
               HB_IDLESLEEP(.1)
             endif
*/
             ret := rsread( 3, @ack, len(ack) ) = len(ack)
             ack := s + ack
/*
           elseif df_ver<7 .and. len(rtr)>5
             HB_IDLESLEEP(.1)
*/
           endif
        endif
     endif
     if ret .or. !disp
        exit
     endif

***********************

     hb_comFlush( 3, 3 )
     hb_idlestate()

     l+=rtr+';'+ack

     begin sequence
     if rswrite(3,chr(27)+chr(0x94))#2 .or. hb_comRecv(3,@s,1,D_TIMEOUT)#1 .OR. asc(S)>=0x80
        break
     endif
     S:=ASC(S)
     if s%128>=64
        l+=";BRAK WYóWIETLACZA KLIJENTA."
        disp:=.t.
     endif
     if s%64>=32
        l+=";NAST§PIùO ZABLOKOWANIE NAZWY TOWARU W PARAGONIE."
        rtr:=NIL
        disp:=.t.
     endif
     if s%32>=16
        l+=";SKASOWANY CMOS."
        rtr:=NIL
        disp:=.t.
     endif
     if s%16>=8
        l+=";ZALEGùY RAPORT DOBOWY."
        disp:=.t.
     endif
     if s%8>=4
        l+="W PAMI®CI FISKALNEJ DRUKARKI ZOSTAùO MAùO MIEJSCA."
        disp:=.t.
     endif
     if s%4>=2
        l+=";W PAMI®CI DRUKARKI ZNAJDUJE SI® DOKUMENT DO WYDRUKOWANIA."
     endif
     if s%2=1
        l+=";BRAK WOLNEGO MIEJSCA W BAZIE KONTRLONEJ NAZW I STAWEK."
        rtr:=NIL
        disp:=.t.
     endif
     if rswrite(3,chr(27)+chr(0x9b))#2 .or. hb_comRecv(3,@s,1,D_TIMEOUT)#1
        break
     endif
     S:=ASC(S)
     if s%8>=4
        l+=';MODUù FISKALNY W TRYBIE "TYLKO ODCZYT"!'
        rtr:=NIL
        disp:=.t.
     endif
     if rswrite(3,chr(27)+chr(149))#2 .or. hb_comRecv(3,@s,1,D_TIMEOUT)#1 .OR. S>=chr(0x80)
        break
     endif
     S:=ASC(S)
     if s%128>=64
        l+=";WYDRUK PARAGONU ZATRZYMANY Z POWODU BRAKU PAPIERU."
        disp:=.t.
     endif
     if s%32>=16
        l+=";NAST§PIùO UNIEWAΩNIENIE PARAGONU."
        disp:=.t.
     endif
     if s%16>=8
        l+=";ZA NISKIE NAPI®CIE AKUMULATORA - POWIADOM SERWIS."
        disp:=.t.
     endif
     if s%8>=4
        l+=";AWARIA MECHANIZMU DRUKARKI."
        disp:=.t.
     endif
     if s%4>=2
        l+=";BRAK PAPIERU."
        disp:=.t.
     endif
     if s%2=1
        l+=";W BUFORZE DRUKOWANIA S§ ZNAKI DO WYDRUKOWANIA."
     endif

     recover
        l+=";BRAK KOMUNIKACJI."
        ret:=.f.
        rtr:=NIL
        s:=3
     end sequence

     IF !ret .AND. s%4<2
        if "DO WYDR" $ l
           tone(130,3)
           if 1=alarm("Bù§D DRUKARKI PARAGON‡W!"+l+";CZY PR‡BOWAè JESZCZE RAZ?",{"TAK","NIE"})
              s:=' '
              if rswrite(3,chr(27)+"*")=2 .and. hb_comRecv(3,@s,1,D_TIMEOUT)=1 .and. s=chr(6)
                 alarm("PONAWIAM WYDRUK!")
              endif
              l:=""
              x:=seconds()
              loop
           endif
           exit
        endif
        if "RAPORT D" $ l
           tone(130,3)
           if 1=alarm("Bù§D DRUKARKI PARAGON‡W!"+l+";CZY PR‡BOWAè JESZCZE RAZ?",{"TAK","NIE"})
              if rswrite(3,chr(27)+"%")=2 .and. hb_comRecv(3,@s,1,D_TIMEOUT)=1 .and. s=chr(6)
                 alarm("DRUKUJ® ZALEGùY RAPORT DOBOWY!")
              endif
              l:=""
              x:=seconds()
              loop
           endif
           exit
        endif
     ENDIF

     if empty(l)
        exit
     endif

     tone(130,3)
     if !RET .AND. !empty(rtr)
         if 1=alarm("Bù§D DRUKARKI PARAGON‡W!"+l+";CZY PR‡BOWAè JESZCZE RAZ?",{"TAK","NIE"})
          x:=seconds()
          l:=""
          loop
         endif
     elseif !empty(l)
        alarm("DRUKARKA PARAGON‡W:"+l)
     endif
********************
     exit

     enddo

     if ndclose
       rsclose(3)
     endif
return RET
   #endif
  #endif
#endif
*******************
#undef EVLINE
#define EVLINE(buf,lbl,lc) while j>0 .and. j<=lc .and. (j=1 .or. valtype(buf[j-1])#"C" .or. buf[j-1]<>lbl);
   ;self:=evline(buf,j++,@x);
   ;IF self<>NIL;
     ;IF self[3]<>NIL;
        ;x:=Self[3];
        ;PRIVATE &x;
     ;END;
     ;x:=&(self[1]);
   ;END;
;ENDDO
*****************************
func w_zes(it_zesmnu)
memvar _sbnorm,defa,strona
memvar j,while,for,a,b,c,d,e,f,g,h,skip,getlist,self,buf
field baza,order,nr_zes,nazwa,pola,relacje,druk_proc
local i,k,l,win,ap,txt,jh,jf,jl,jt,el,x,y
static apcomp:={}
#ifndef A_XPRN
#define P_ROWN 58
#else
#ifdef A_PCL
#define P_ROWN if(landscape,p_rownl,p_rown)
#define P_COLN if(landscape,p_colnl,p_coln)
#endif
memvar p_rown
#endif

if !file("zes_def.dbf")
   RETURN .f.
ENDIF
sel("zes_def")
if it_zesmnu#NIL
   goto it_Zesmnu
endif
setpos(row()+recno(),col())
if szukam({1,col(),,,0,0,"Zestawienia",{||nr_zes+" "+nazwa}})
   it_zesmnu:=recno()
   ap:=getlines(POLA)
   l:=len(ap)
   k:=0
   for i:=1 to l
      if ap[i]<>'&:'
        k+=1
        ap[i]:=asize(getlines(ap[i],";"),6)
      endif
   next
   win:=window(k,60,_sbnorm)
   @ win[1],win[2]+2 say trim(nazwa)
   private getlist:={}
   select 0
   k:=0
   for i:=1 to l
      if valtype(ap[i])$'MC'
        (&(trim(subs(ap[i],3))),txt:=NIL)
      else
        k+=1
        if ZES_DEF->baza#"MEMVAR"
          j:=(ap[i,2])
          PRIVATE &j
          &j:=ZES_DEF->&j
        endif
        @ win[1]+k,win[2]+1 say padl(ap[i,1],15)
        txt:=GETnew(row(),col()+1,memvarblock(ap[i,2]),ap[i,2])
        if !empty(ap[i,5])
           txt:preblock:=&(trim((ap[i,5])))
        endif
        if !empty(ap[i,4])
           txt:postblock:=&(trim((ap[i,4])))
        endif
        if !empty(ap[i,3])
           txt:picture:=ap[i,3]
        endif
        if ZES_DEF->(type(ap[i,2]))=if(ZES_DEF->baza="MEMVAR","C" ,"M")
           txt:cargo:=.t.
           if empty(txt:picture)
              txt:picture:='@S45'
           endif
        endif
        aadd(getlist,txt)
     endif
   next
   aeval(getlist,{|g|g:display()})
   READ
   //readmodal(getlist)
   select zes_def
   window(win)
   if readkey()#27
      begin sequence
      if ZES_DEF->baza#"MEMVAR"
         lock recno()
         for i:=1 to l
          if valtype(ap[i])='A'
           FIELD->&(ap[i,2]):=MEMVAR->&(ap[i,2])
          endif
         next
         sel(baza,trim(zes_def->order))
         rel(make_subar(getlines(zes_def->relacje),4))
         unlock in zes_def
         go top
      else
         select 0
      endif
      private while:={||!eof()}
      private skip,for
#ifdef B_SKIP
      skip:=B_SKIP
#else
      skip:={||dbskip()}
#endif
      private a,b,c,d,e,f,g,h,j,self,buf

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
             buf:=getlines(memoread(i))
          endif
        else
          buf:=getlines(txt)
          txt:=NIL
        endif
        aadd(apcomp,{zes_def->(recno()),buf})
      endif
#ifdef D_HWPRN
      oprn:=D_HWPRN
#endif
      l:=len(buf)
      cls
      j:=1
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
         if j<2 .or. j>l
            exit
         endif
         jl:=j
         do while y .and. prow()<P_ROWN // .or. !set(_SET_PRINTER))
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
         enddo
         if !y
            j:=if(jt=NIL,ascan(buf,{|x|valtype(x)="C".and. x=":TOTAL"})+1,jt)
            exit
         endif
         if jf=NIL
            jf:=ascan(buf,{|x|valtype(x)="C".and. x=":FOOTER"})+1
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

         ?? spec(chr(13)+chr(12))
         setprc(0,0)

      enddo
      if strona=0 .or. j<2
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
***************
****************************************************************
#else
****************************************************************
***************

#ifdef A_LPTN
#define D_LPT dok_par[MAG_POZ,r,A_LPTN]
#else
#define D_LPT
#endif
#define dok_p_r dok_par[MAG_POZ,r,1]
#define dok_zew dok_par[MAG_POZ,r,2]
#define dok_kon dok_par[MAG_POZ,r,3]
#define dok_war dok_par[MAG_POZ,r,4]
#define dok_lpm dok_par[MAG_POZ,r,5]
#define dok_fot dok_par[mag_poz,r,7]
#define dok_pie dok_par[mag_poz,r,10]
#define dok_spc dok_par[mag_poz,r,11]
#define dok_naz dokumenty[MAG_POZ,r]
#define dok_ew  dok_par[MAG_POZ,r,8]
#define dok_df  dok_par[MAG_POZ,r,A_DF]
#ifdef A_IZ
field ilosc_f
#else
#define ilosc_f ilosc
#endif
*
*****************
procedure wydruk_dok(dok_kop)

#ifdef A_XPRN
memvar p_4xon,p_4xoff,p_coln,p_bon,p_boff,p_uon,p_uoff,p_halfpage
#define P_HALFPAGE eval(p_halfpage,24)
#else
#ifdef A_PCL
#define P_4XON  chr(27)+'(s24v5H'
#define P_4XOFF chr(27)+'(s12v10H'
#define P_COLN  78
#define P_BON   chr(27)+"(s3B"
#define P_BOFF  chr(27)+"(s0B"
#define P_UON   chr(27)+"&"+"d0D"
#define P_UOFF  chr(27)+"&"+"d@"
#else
#define P_4XON  chr(27)+'w1'+chr(27)+'W1'
#define P_4XOFF chr(27)+'w0'+chr(27)+'W0'
#define P_COLN  80
#define P_BON   chr(27)+'G'
#define P_BOFF  chr(27)+'H'
#define P_UON   chr(27)+'-1'
#define P_UOFF  chr(27)+'-0'
#define P_HALFPAGE chr(27)+"C"+chr(24)
#endif
#endif

#ifdef A_WA
field wartosc
#else
#ifdef A_FA
field wartosc
#endif
#endif

memvar adres_mag,magazyny,dokumenty,dok_par,operator,firma_n,firma_a,;
       changed,mag_poz,mag_biez,stawki,oprn

local txt1,txt2,txt3,r,i,j,k,s,w,p,bc,bw,x,;
  ilj,was,v,wt,vt,rec,fakKorFlag,wp10,wp17,wp12,cpi,mes,gt,pm,u,ut

#ifdef A_MM
  if empty(mag_biez)
     mag_biez:=left(magazyny[max(1,mag_poz)],2)
  endif
#else
    private mag_biez:=nr_mag
#endif
    private mag_poz:=max(1,ascan(magazyny,mag_biez))
  IF dok_kop=NIL
    dok_kop=1
    tone(262,2)
    if 2#alarm("CZY DRUKOWAè",{"NIE","TAK"})
      return
    endif
  ENDIF

#ifdef D_HWPRN
oprn:=D_HWPRN
#command ?  [<exp,...>]         => wQ()[;?? <exp>]
#command ?? <exp1> [,<expn>]    => wqq(<exp1>)[;wQQ(<expn>)]
#command TEXT <st> => aeval(getlines(strtran(<st>,";",nl)),{|x|wq(x)})
#else
#command ? [<List,...>] => qout() [;?? <List>]
#command ?? <exp1> [,<expn>]    => qqout(<exp1>)[;QQout(<expn>)]
#command TEXT <st> => aeval(getlines(strtran(<st>,";",nl)),{|x|qout(x)})
#endif
  begin sequence
  mes:=message("Prosz© czekaÜ;TRWA WYDRUK.")
  select main
#ifndef STANY
        SET RELATION TO INDEX INTO INDX_MAT
#else
        SET RELATION TO NR_MAG+INDEX INTO INDX_MAT
#endif
        SET ORDER TO "MAIN_NRK"

  select dM
#ifdef A_LAN
  go recno()
  LOCK recno()
#endif
    r:=max(1,ascan(dokumenty[MAG_POZ],smb_dow D_SUBDOK))

  set console off

  print(D_LPT)
  setprc(0,0)

    IF KTO_PISAL=CHR(255)
      KTO_PISAL:=OPERATOR
      changed:=.t.
    ENDIF     


  do case
    case dok_naz="K"
      txt1:="   Pow¢d:"
      txt2:="Dok. popraw. :"
      txt3:="wystaw. dnia :"
#ifdef A_FA
    case dok_p_r="F"
      if subs(dok_naz,2)="K"
      txt2:="Dok. popraw. :"
      txt3:="wystaw. dnia :"
#ifdef A_ODDO
      elseif dok_naz#"D"
      txt2:="    Za okres :"
      txt3:="           do"
#endif
      else
      txt2:="Nr zam¢wienia:"
      txt3:="Data sprzedaæy"
      endif
#endif
    case dok_p_r#"P"
      txt1:="Odbiorca:"
#ifdef A_ODDO
      txt2:="              "
#else
      txt2:="Dok. wydania :"
#endif
      txt3:="Towar wyd. dn:"
    otherwise
      txt1:="Dostawca:"
      txt2:="Nr fakt/rach.:"
      txt3:="Towar przyj dn"
  endcase

  pm:=if(dok_p_r="P",1,-1)

  for i:=1 to dok_kop
    message(1)
#ifdef A_FA
    if dok_p_r="F"
       ?? padl(left(firma_a,1+at(", ",firma_a))+dtoc(data),P_COLN)
       TEXT dok_pie
#ifdef A_FULNUM
       ? spec(P_BON+P_4XON),padc(subs(trim(dok_naz),4)+" "+ltrim(KEY_DOK)+ltrim(nr_dowodu),P_COLN/2),spec(P_BOFF+P_4XOFF)
#else
       ? spec(P_BON+P_4XON),padc(subs(trim(dok_naz),4)+" "+ltrim(nr_dowodu)+"/"+str(year(data)%100,2),P_COLN/2),spec(P_BOFF+P_4XOFF)
#endif

        SELECT firmy
       set order to "FIRM_NUM"
       if ""#dok_kon .and. dbSEEK(dM->(D_KH),.f.)
          ? numer_kol
          ? "Pàatnik: ",cpad(nazwa,40)
          ?? " "+txt2+" "+left(dM->nr_faktury,14)
          ? "  Adres: ",cpad(adres,P_COLN-35)
          if ident#"   "
             ? "    NIP: "+pad(ident,P_COLN-43)
          endif
        ELSE
          ?
          ? "Pàatnik: ",CPAD(dM->dost_odb,P_COLN-40)
          if dok_kon="?" .and. dok_zew="V"
             ?? "            NIP: "
          else
             ?? " "+txt2+" "
          endif
          ?? left(dM->nr_faktury,14)
          ? SPACE(P_COLN-25)
       endif
       select dM
       ?? txt3+" ",data_dost
#ifdef A_FAT
       if subs(dok_naz,2)#"K"
          ?
          ? "Numer Specyfikacji: "+nr_spec+" órodek transportu: "+transport
       endif
#endif
    message(1)
    ? ccpi(7)
#ifdef A_SWW
    ? "Lp|  Nazwa towaru                            | Kod SWW|Symbol|"
#else
    ? "Lp  Nazwa towaru lub usàugi                      "
#ifdef A_SHORTIND
    ?? "| Kod|"
#else
#ifdef A_KTM
#ifdef A_SWWKTM
    ?? "| Kod SWW|"
#else
    ?? "| Kod KTM / SWW  |"
#endif
#else
#ifdef A_OLZA
    ?? "| Kod SWW      |"
#else
    ?? "| Kod SWW    |"
#endif
#endif
#endif
#endif
    wp17:=pcol()
    if dok_zew="V"
#ifdef A_CENVAT
       ?? ccpi(8)+"   IloòÜ  |Jedn| Cena jedn.|WartoòÜ net.| Cena z VAT| % i kwota  VAT| Wart. z VAT",ccpi(7)
#else
       ?? "   IloòÜ  |Jedn| Cena jedn.|WartoòÜ net.| % i kwota VAT | Wart. z VAT"
#endif
    elseif dok_zew="U"
       ?? "   IloòÜ  |Jedn|Cena brutto|Wart. brutto|  W tym VAT    |Wart. netto "
    else
       ?? "   IloòÜ  |Jedn|Cena jedn.|  WartoòÜ"
    endif
    do while .t.
    ut:=wt:=vt:=0
    p:=D_LP0
#ifndef A_WA
  #define wartosC ilosc*(s)->cenA
#endif
#ifdef A_GRAM
    gt:=0
#endif
#ifdef A_JMTOT
    ilj:={}
#endif
    was:={}
    ? ccpi(4)+repl("-",79)
    if fakkorflag#NIL
       ? "Korekta:"
       go fakkorflag
       fakkorflag:=NIL
    elseif subs(dok_naz,2)="K"
       fakkorflag:=recno()
       if dbSEEK(pad(nr_faktury,D_MM),.f.)
          ? "Przed korekt•:"
       else
          go fakkorflag
          fakkorflag:=NIL
       endif
    endif
    ?? ccpi(7)
    select main
    dbSEEK(dM->(KEY_DOK+NR_DOWODU),.f.)
    DO while KEY_DOK+NR_DOWODU=dM->(KEY_DOK+NR_DOWODU)
      message(1)
      w:=WGR(-ilosc_f,cena,val(proc_vat),dok_df)/100
      v:=ILEVATGR(-ilosc_f,cena,val(proc_vat),dok_df)/100
      s:=i_lam(dM->data)
      ut-=wartosC
      wt+=w
#ifndef A_NVAT
      vt+=v
#endif
#ifdef A_GRAM
      gt-=ilosc_f*(s)->gram/1000
#endif
#ifdef A_JMTOT
      j:=ascan(ilj,{|x|x[1]=(s)->jm})
      if j=0
       aadd(ilj,{(s)->jm,-ilosc_f})
      else
       ilj[j,2]-=ilosc_f
      endif
#endif
      k:=ascan(was,{|x|x[1]=proc_vat})
      if k=0
       aadd(was,{proc_vat,w,v})
      else
       was[k,2]+=w
       was[k,3]+=v
      endif
      p:=pozycja
      cpi:=17
      if subs(dok_naz,2)#"K" .and. p=dM->pozycja .and. wt>0 .and. w<0 // upust
         ? ccpi(4)+repl("-",79),ccpi(7)
      endif
#ifdef A_SWW
      ? str(D_LPVAL(p),2)+"|",cpad((s)->NAZWA,42,@cpi)
      if cpi#17
         ?? spec(chr(13)),space(45)
      endif
      ?? "|"+(s)->sww+"|"+tran(index,"@R "+ INDEXPIC )+"|"
#else
      ? str(D_LPVAL(p),2)+"|",cpad((s)->NAZWA,46,@cpi)
      if cpi#17
         ?? spec(chr(13)),space(49)
      endif
#ifdef A_SWWKTM
      ?? "|"+(s)->sww+"|"
#else
      ?? "|"+tran(index,"@R "+ INDEXPIC )+"|"
#endif
#endif
     if dok_zew="V"
#ifdef A_CENVAT
        ?? ccpi(8)+strtran(str(-ilosc_f,10,3),".000",".   ")+"|"+(s)->jm+"|";
        +strpic(WbezVAT(1,cena,val(proc_vat),dok_df),11,A_ZAOKR,"@E ")+"|"+strpic(w-v,12,A_ZAOKR,"@E ")+"|";
        +strpic(cena,11,A_ZAOKR,"@E ")+"|"+proc_vat+"."
#ifdef A_NVAT
        ?? "      X     |      X",ccpi(7)
#else
        ?? strpic(v,12,A_ZAOKR,"@E ")+"|";
        +strpic(w,12,A_ZAOKR,"@E "),ccpi(7)
#endif
#else
        ?? strtran(str(-ilosc_f,10,3),".000",".   ")+"|"+(s)->jm+"|"+;
        strpic(cena,11,A_ZAOKR,"@E ")+"|"+strpic(w,12,A_ZAOKR,"@E ")+"|";
        +proc_vat+"."
#ifdef A_NVAT
        ?? "      X     |      X"
#else
        ?? strpic(v,12,A_ZAOKR,"@E ")+"|";
        +strpic(w+v,12,A_ZAOKR,"@E ")
#endif
#endif
     elseif dok_zew="U"
#ifdef A_CENVAT
        ?? strtran(str(-ilosc_f,10,3),".000",".   ")+"|"+(s)->jm+"|";
        +strpic(cena,11,A_ZAOKR,"@E ")+"|"+strpic(w,12,A_ZAOKR,"@E ")+"|";
        +proc_vat+"."
#ifdef A_NVAT
        ?? "      X     |      X"
#else
        ?? strpic(v,12,A_ZAOKR,"@E ")+"|"+strpic(w-v,12,A_ZAOKR,"@E ")
#endif
#else
        ?? strtran(str(-ilosc_f,10,3),".000",".   ")+"|"+(s)->jm+"|";
        +strpic(WzVAT(1,cena,val(proc_vat),dok_df),11,A_ZAOKR,"@E ")+"|"+strpic(w+v,12,A_ZAOKR,"@E ")+"|";
        +proc_vat+"."
#ifdef A_NVAT
        ?? "      X     |      X"
#else
        ?? strpic(v,12,A_ZAOKR,"@E ")+"|"+strpic(w,12,A_ZAOKR,"@E ")
#endif
#endif
     else
        ?? strtran(str(-ilosc_f,10,3),".000",".   ")+"|"+(s)->jm+"|";
        +strpic(cena,11,A_ZAOKR,"@E ")+"|"+strpic(w,12,A_ZAOKR,"@E ")
     endif
      skip
    ENDDO
    message(1)
    skip -1
  SELECT dM
      ? ccpi(4)+repl("-",79)
      ?
#ifdef A_FA
#ifdef A_NVAT
      w:=wt
      v:=vt
      wt:=vt:=0
#ifdef A_CENVAT
      aeval(was,{|x|x[2]:=round(100*x[2],0),wt+=x[2],x[3]:=x[2]*val(x[1])/(100+val(x[1])),vt+=round(x[3],0)})
#else
      aeval(was,{|x|x[2]:=round(100*x[2],0),wt+=x[2],x[3]:=x[2]*val(x[1])/100,vt+=round(x[3],0)})
#endif
      vt/=100
      wt/=100
#endif
      if dok_zew$"UV" .and. ROUND(warT_vaT-vt,A_ZAOKR)#0
         if fakkorflag=NIL .and. i=1 .and. (tone(130,3),if(changed,1=alarm("NIEZGODNA WARTOóè PODATKU;CZY DOKONAè KOREKTY ?",{"TAK","NIE"},2,2),NIL=alarm("NIEZGODNA WARTOóè PODATKU;NIE DOKONANO KOREKTY !!!")))
             wart_vat:=vt
             for k:=1 to len(stawki)
                if val(stawki[k])#0
                   x:=ascan(was,{|x|x[1]=stawki[k]})
                   if x#0
                      x:=was[x,3]
                   endif
                   field2biN(fieldpos(D_NVAT+ltrim(stawki[k])),x)
                endif
             next k
         else
             vt:=wart_vat
             for k:=1 to len(stawki)
                if val(stawki[k])#0 .and. (n:=fieldpos(D_NVAT+ltrim(stawki[k])))#0
                   x:=ascan(was,{|x|x[1]=stawki[k]})
                   if x=0
                      if bin2D(fieldget(n))#0
                         aadd(was,{stawki[k],0,bin2D(fieldget(n))})
                      endif
                   else
                      was[x,3]:=bin2D(fieldget(n))
                   endif
                endif
             next k
         endif
      endif
      if dok_ew#"E" .and. ROUND(wartosc-wt,A_ZAOKR)#0
      if fakkorflag=NIL .and. i=dok_kop .and.changed
         WARTOSC:=wt
         #ifdef A_DF
         field2bin('d_wartosc',w)
         #endif
         //changed:=.t.
         alarm("WARTOóè SUMY POZYCJI NIEZGODNA Z INFORMACJ§ W NAGù‡WKU;DOKONANO KOREKTY",,,3)
      else
         alarm("NIEZGODNA WARTOóè SUMY POZYCJI;NIE DOKONANO KOREKTY !!!",,,3)
         wt:=WARTOSC
      endif
      endif
#ifdef A_NVAT
      aeval(was,{|x|x[2]/=100,x[3]:=round(x[3],0)/100})
#endif
#endif
#ifdef A_WE
      if ROUND(warT_ewiD-pm*ut,A_ZAOKR)#0
        changed:=.t.
        alarm("WARTOóè EWIDENCYJNA SUMY POZYCJI NIEZGODNA Z INFORMACJ§ W NAGù‡WKU;DOKONANO KOREKTY",,,3)
        warT_ewiD:=pm*ut
      ENDIF
#endif
      if POZYCJA#P
        changed:=.t.
        alarm("ILOóè POZYCJI NIEZGODNA Z INFORMACJ§ W NAGù‡WKU;DOKONANO KOREKTY",,,3)
        pozycja:=p
      ENDIF
      if p>D_LP1
#ifdef A_GRAM
        ?? "WAGA: "+ltrim(str(gt,10,2))+" kg",spec(chr(13))
#endif
        ?? ccpi(7)+space(wp17-11),spec(P_BON),cpad("RAZEM:",11,17,1),spec(P_BOFF)
        if len(was)>1
          if dok_zew#"V"
#ifdef A_CENVAT
             ?? space(27),spec(P_BON),"|"+strpic(wt,12,A_ZAOKR,"@E ")
             if dok_zew="U"
                ?? "|  "+strpic(vt,13,A_ZAOKR,"@E ")+"|"+strpic(wt-vt,12,A_ZAOKR,"@E ")
             endif
#else
             ?? space(27),spec(P_BON),"|"+strpic(wt+vt,12,A_ZAOKR,"@E ")
             if dok_zew="U"
                ?? "|  "+strpic(vt,13,A_ZAOKR,"@E ")+"|"+strpic(wt,12,A_ZAOKR,"@E ")
             endif
#endif
          else
#ifdef A_CENVAT
             ?? ccpi(8),space(27),spec(P_BON),"|"+strpic(wt-vt,12,A_ZAOKR,"@E ");
             +"|              "+strpic(vt,13,A_ZAOKR,"@E ")+"|"+strpic(wt,12,A_ZAOKR,"@E "),ccpi(7)
#else
             ?? space(27),spec(P_BON),"|"+strpic(wt,12,A_ZAOKR,"@E ");
             +"|  "+strpic(vt,13,A_ZAOKR,"@E ")+"|"+strpic(wt+vt,12,A_ZAOKR,"@E ")
#endif
          endif
          ? spec(P_BOFF),space(wp17-11),cpad("w tym:",11,17,1)
        endif
#ifdef A_JMTOT
      for j=1 to max(len(ilj),len(was))
        if j<=len(ilj)
#ifdef A_CENVAT
           if dok_zew="V"
              ?? ccpi(8)
           endif
#endif
           ?? strtran(str(ilj[j,2],10,3),".000",".   ")+"|"+ilj[j,1]+"|"+space(11)
        else
#ifdef A_CENVAT
           if dok_zew="V"
              ?? ccpi(8)
           endif
#endif
           ?? space(27)
        endif
        if j<=len(was)
#else
      for j=1 to len(was)
#ifdef A_CENVAT
           if dok_zew="V"
              ?? ccpi(8)
           endif
#endif
           ?? space(27)
#endif
        if dok_zew="V"
#ifdef A_CENVAT
           ?? strpic(was[j,2]-was[j,3],13,A_ZAOKR,"@E ")+"|           |"+;
           was[j,1]+"."+strpic(was[j,3],12,A_ZAOKR,"@E ")+"|"+strpic(was[j,2],12,A_ZAOKR,"@E "),ccpi(7)
#else
           ?? strpic(was[j,2],13,A_ZAOKR,"@E ")+"|"+;
           was[j,1]+"."+strpic(was[j,3],12,A_ZAOKR,"@E ")+"|"+strpic(was[j,2]+was[j,3],12,A_ZAOKR,"@E ")
#endif
        elseif dok_zew="U"
#ifdef A_CENVAT
           ?? strpic(was[j,2],13,A_ZAOKR,"@E ")+"|"+was[j,1]+"."+strpic(was[j,3],12,A_ZAOKR,"@E ")+"|"+strpic(was[j,2]-was[j,3],12,A_ZAOKR,"@E ")
#else
           ?? strpic(was[j,2]+was[j,3],13,A_ZAOKR,"@E ")+"|"+was[j,1]+"."+strpic(was[j,3],12,A_ZAOKR,"@E ")+"|"+strpic(was[j,2],12,A_ZAOKR,"@E ")
#endif
        else
           ?? strpic(was[j,2],13,A_ZAOKR,"@E ")
        endif
#ifdef A_JMTOT
        endif
#endif
        ? space(wp17)
      next
        ?? ccpi(4)
      endif
      if fakKorFlag#NIL
          loop
      endif
      exit
      enddo
      TEXT if(dok_spc="&:",&(trim(subs(dok_spc,3))),dok_spc)
      ?
      if ""#uwagi
         TEXT uwagi
         ?
      endif
#ifdef A_NOTOT
     if dok_naz#"D"
#endif
     if dok_zew$"UV"
#ifdef A_CENVAT
      ? padl("WARTOóè NETTO"+strpic(wt-vt,15,A_ZAOKR,"@E "),35)+" zà."
#else
      ? padl("WARTOóè NETTO"+strpic(wt,15,A_ZAOKR,"@E "),35)+" zà."
      wt+=vt
#endif
      ?
      ? padl("+ VAT "+strpic(vt,15,A_ZAOKR,"@E "),35)+" zà."
      ? padl("----- ---------------",35)
     endif
      wt:=ROUND(wt,A_ZAOKR)
      ? spec(P_BON),padl("DO ZAPùATY "+strpic(wt,15,A_ZAOKR,"@E "),35)+" zà.",spec(P_BOFF)
      ?
      ? "sàownie:",cpad(slownie(wt),70,,0)
      ?
      ? "Zapàata "
      if przelewem=0 .and. czekiem=0 .and. termin_p=data
         ?? "got¢wk•."
         ?
         ?
      else
      if wt#czekiem+przelewem
         ?? "  got¢wk•: "+strpic(wt-przelewem-czekiem,15,A_ZAOKR,"@E "),"zà."
         ?
         ?
      else
         ?? spec(chr(13))
      endif
      if przelewem#0
         ?? "        przelewem: "+strpic(przelewem,15,A_ZAOKR,"@E "),"zà."
         ?
         ?
      endif
      if czekiem#0
         ?? "            kart•: "+strpic(czekiem,15,A_ZAOKR,"@E "),"zà."
         if !empty(nr_czeku)
            ?? "  Nr karty: "+trim(nr_czeku)
         endif
         ?
         ?
      endif
      endif
#ifdef A_NOTOT
      endif
#endif
      TEXT if(dok_fot="&:",&(trim(subs(dok_fot,3))),dok_fot)
      ?? spec(chr(13)+chr(12))
    else
#endif

************************************************
#ifdef P_HALFPAGE
   ?? spec(P_HALFPAGE)
#endif
#ifndef A_KOMOR
#ifdef A_DIETA
#ifdef A_SHORTIND
#define A_KOMOR
#endif
#endif
#endif
    ?? padr(firma_n,P_COLN-29),padl(cdow(dM->data),12)," dnia ",dM->data
    ? magazyny[mag_poz]
    ? adres_mag[mag_poz]
    ?
    ? space(25),spec(P_BON+P_4XON),dM->smb_dow,spec(P_BOFF+P_4XOFF),padl(substr(dok_naz,3)+"   Numer kolejny: "+dM->nr_dowodu,P_COLN-29)
    if dok_naz="K"
        ?
        ? txt1,' ',CPAD(dM->dost_odb,40),txt2,' ',left(dM->nr_faktury,14)
        ? SPACE(P_COLN-26),txt3,' ',dM->data_dost
    elseif dok_zew#"W"
        SELECT firmy
        set order to "FIRM_NUM"
        if dbSEEK(dM->(D_KH),.f.)
          ? numer_kol
          ? txt1,' ',cpad(nazwa,P_COLN-40),txt2,left(dM->nr_faktury,14)
          ? "   Adres: ",cpad(adres,P_COLN-35)
#ifdef A_VAT
          if dok_zew$"UV" .and. ident#"   "
             ? "   Identyfikator: ",pad(ident,P_COLN-43)
          endif
#endif
        ELSE
          ?
          ? txt1,' ',CPAD(dM->dost_odb,40),' ',txt2,' ',left(dM->nr_faktury,14)
          ? SPACE(P_COLN-25)
        endif
        ?? txt3,' ',dM->data_dost
    else
          ?
          ? txt1,' ',dM->dost_odb
#ifdef A_OLZA
      if ""#dok_kon
         ?
         ? "Konto koszt¢w: ",dM->konto_kosz,' ',space(P_COLN-48),' ',"Stanowisko koszt¢w: ",dM->stano_kosz
      endif
#endif
    ENDIF
    message(1)
    select main
    dbSEEK(dM->(KEY_DOK+NR_DOWODU),.f.)
#ifdef A_WA
  #define ceNA wartosc/ilosc
#else
  #undef wartosC
  #define wartosC ilosc*(s)->cenA
  #define ceNA (s)->cenA
#endif

#ifdef A_KOMOR
    if dok_zew="W" .and. mag_biez=A_MAGDI
       ilj:=0
       was:=index
       ? "                       (suma)",spec(P_UON)
    else
       ? spec(P_UON)
    endif
#else
    ? spec(P_UON)
#endif
wp17:=0
#ifdef A_SHORTIND
#ifdef A_JMO
#ifdef A_OLZBY
    ? "Lp|Nazwa wyrobu     opakowanie|"
#else
    ? "Lp|Peàne okreòlenie materiaàu |"
#endif
#ifdef A_GRAM
    if dok_war#"-" .or. ""#dok_kon
       wp10:=pcol()
       ?? ccpi(5)
    endif
#endif
    ?? " Kod|"+if(""#dok_kon,"Konto |","")
#ifdef A_GRAM
    ?? " Waga kg |"
#endif
#else
    ? "Lp|Peàne okreòlenie materiaàu | Kod|"+if(""#dok_kon,"Konto |","")
#endif
    if dok_war="-"
    ?? "   IloòÜ  |Jedn"
    else
#ifdef A_PCL
    ?? "Cena jedn|   IloòÜ  |Jedn|WartoòÜ  "
    #define D_CENLTH 9
#else
    #define D_CENLTH 10
    ?? "Cena jedn.|   IloòÜ  |Jedn|WartoòÜ   "
#endif
    endif
#ifdef A_JMO
#ifdef A_GRAM
    if dok_war#"-" .or. ""#dok_kon
    wp12:=pcol()-wp10
    ?? ccpi(4)
    else
#endif
#endif
    wp10:=pcol()
    wp12:=0
#ifdef A_JMO
#ifdef A_GRAM
    endif
#endif
#endif
    ?? spec(P_UOFF)
#else
#ifdef A_SWW
    ? "Lp    Nazwa wyrobu            "
    wp10:=pcol()
    ?? ccpi(5),"|  SWW   |Symbol|"+if(""#dok_kon,"Konto |","")+"Cena jedn|   IloòÜ  |Jedn|"
    #define D_CENLTH 9
    if dok_war#"-"
    ?? "WartoòÜ  "
    endif
#else
#ifdef A_KTM
    ? "Lp Peàne okreòlenie materiaàu"
    wp10:=pcol()
    ?? ccpi(5),"| Kod materiaàu  |"
#ifdef A_OBR
    ?? if(""=dok_kon,"Cena jedn.|",if(dok_zew="W","Konto           |","Zam¢wienie   |"))+"   IloòÜ  |Jedn|"
    #define D_CENLTH 10
    if dok_war#"-"
    ?? "WartoòÜ   "
    endif
#else
    ?? if(""#dok_kon,"Konto |","")+"Cena jedn|   IloòÜ  |Jedn|"
    #define D_CENLTH 9
    if dok_war#"-"
    ?? "WartoòÜ  "
    endif
#endif
#else
    ? "Lp|Peàne okreòlenie materiaàu |"
    wp10:=pcol()
    ?? ccpi(5)
#ifdef A_OLZA
    ?? " Kod materiaàu|"
    if ""#dok_kon
       ?? ccpi(7),"Zlecenie |",ccpi(5)
       wp17:=10
    endif
    ?? "    Cena |   IloòÜ  |Jedn|"
    #define D_CENLTH 9
    if dok_war#"-"
    ?? "WartoòÜ  "
    endif
#else
    ?? " Kod mater. |"+if(""#dok_kon,"Konto |","")+"Cena jedn.|   IloòÜ  |Jedn|"
    #define D_CENLTH 10
    if dok_war#"-"
    ?? "WartoòÜ   "
    endif
#endif
#endif
#endif
    wp12:=pcol()-wp10-wp17
    ?? ccpi(4),spec(P_UOFF)
#endif
#ifdef A_FA
    wt:=vt:=0
    was:={}
#endif
#ifdef A_GRAM
    gt:=0
#endif
    ut:=0
    p:=D_LP0
    DO while KEY_DOK+NR_DOWODU=dM->(KEY_DOK+NR_DOWODU)
      message(1)
      s:=i_lam(dM->data)
#ifdef A_FA
      v:=VATPZ(pm*cena,val(proc_vat))
      wt+=w:=pm*cena
#ifndef A_NVAT
      vt+=v
#endif
      k:=ascan(was,{|x|x[1]=proc_vat})
      if k=0
       aadd(was,{proc_vat,w,v})
      else
       was[k,2]+=w
       was[k,3]+=v
      endif
#endif
      ut+=w:=pm*wartosC
      p:=pozycja
      j:=pm*ilosc
#ifdef A_GRAM
      gt+=j*(s)->gram/1000
#endif
#ifdef A_JMO
#define D_ILOUT if(abs(j)>=(s)->przel,;
                     if(j%(s)->przel=0,;
                           str(j/(s)->przel,6)+"    ",;
                           stuff(str(int(j/(s)->przel)+j%(s)->przel/1000,10,3),7,1,"r");
                       ),;
                     strtran(str(j,10,3),".000",".   ");
                  )
#define jM (if(abs(j)>=przel,jm_opcja,jm))
#else
#define D_ILOUT strtran(str(j,10,3),".000",".   ")
#endif
#ifdef A_SHORTIND
#ifdef A_KOMOR
      if was#NIL
        v:=(s)->NAZWA
        ilj+=j
        ? space(30)
      else
        cpi:=10
        ? str(D_LPVAL(p),2)+"|",cpad((s)->NAZWA,27,@cpi)
        if cpi#10
           ?? spec(chr(13)+space(30))
        endif
      endif
      ?? "|"+index+"|"
      if ""#dok_kon
        ?? cpad(nr_zlec,6,,1),"|"
      endif
      ?? STRPIC(ceNA,D_CENLTH,A_ZAOKR,"@E ",.t.)
#else
      cpi:=10
      ? str(D_LPVAL(p),2)+"|",cpad((s)->NAZWA,27,@cpi)
      if cpi#10
         ?? spec(chr(13)),space(30)+"|"
      else
         ?? "|"
      endif
#ifdef A_JMO
#ifdef A_GRAM
    if dok_war#"-" .or. ""#dok_kon
       ?? ccpi(5)
    endif
#endif
#endif
      ?? index
      if ""#dok_kon
         ?? "|",cpad(nr_zlec,6,,1)
      endif
#ifdef A_GRAM
      ?? "|"+tran(j*(s)->gram*.001,"@Z ## ###.##")
#endif
      if dok_war#"-"
         ?? "|"+STRPIC(ceNA,D_CENLTH,A_ZAOKR,"@E ",.t.)
      endif
#endif
#else
#ifdef A_KTM
      cpi:=10
      ? p+"|",cpad((s)->NAZWA,27,@cpi)
      if cpi#10
         ?? spec(chr(13)),space(29)
      endif
      ?? ccpi(5)+"|"+tran(index,"@R "+ INDEXPIC )+"|"
#ifdef A_OBR
      ?? if(""#dok_kon,strpic(ceNA,10,A_ZAOKR,"@E "),if(dok_zew="W",tran(nr_zlec,"@R XXX/XXX/XX/XXXXX"),nr_zlec))
#else
      ?? if(""#dok_kon,nr_zlec+"|","")+STRPIC(ceNA,D_CENLTH,A_ZAOKR,"@E ",.t.)
#endif
#else
#ifdef A_OLZA
        cpi:=10
        ? str(D_LPVAL(p),2)+"|",cpad((s)->NAZWA,27,@cpi)
        if cpi#10
           ?? SPEC(chr(13)),space(30)
        endif
      ?? "|",ccpi(5)+tran(index,"@R "+ INDEXPIC )+"|"
      if ""#dok_kon
         ?? ccpi(7)+nr_zlec+"|",ccpi(5)
      endif
      ?? STRPIC(ceNA,D_CENLTH,A_ZAOKR,"@E ",.t.)
#else
#ifdef A_SWW
        cpi:=10
        ? str(D_LPVAL(p),2)+"|",cpad((s)->NAZWA,27,@cpi)
        if cpi#10
           ?? spec(chr(13)+space(30))
        endif
      ?? ccpi(5),"|"+(s)->sww+"|"+tran(index,"@R "+ INDEXPIC )+"|"+if(""#dok_kon,nr_zlec+"|","")+STRpic(ceNA,D_CENLTH,A_ZAOKR,"@E ",.t.)
#else
        cpi:=10
        ? str(D_LPVAL(p),2)+"|",cpad((s)->NAZWA,27,@cpi)
        if cpi#10
           ?? spec(chr(13)+space(30))
        endif
      ?? "|",ccpi(5),tran(index,"@R "+ INDEXPIC )+"|"+if(""#dok_kon,nr_zlec+"|","")+STRpic(ceNA,D_CENLTH,A_ZAOKR,"@E ",.t.)
#endif
#endif
#endif
#endif
      ?? "|",spec(P_BON),D_ILOUT,spec(P_BOFF)+"|"+(s)->jM
      if dok_war#"-"
         ?? "|"+strpic(w,D_CENLTH,A_ZAOKR,"@E ")
      endif
      ?? ccpi(4)
      skip
#ifdef A_KOMOR
      if was#NIL
      if index#was .or. KEY_DOK+NR_DOWODU#dM->(KEY_DOK+NR_DOWODU)
        ?? spec(chr(13)),str(D_LPVAL(p),2)+"|",cpad(v,20,10,1),spec(P_BON),strtran(str(ilj,7,3),".000",".   "),spec(P_BOFF)
        was:=index
        ilj:=0
      else
        ?? spec(chr(13)),str(D_LPVAL(p),2)+"|",cpad(v,27,10,2)
      endif
      endif
#endif
    ENDDO
    message(1)
    skip -1
  SELECT dM
      ?? spec(chr(13)+P_UON),space(wp10+wp12/1.2+wp17/1.66)
      ? spec(P_UOFF)
#ifdef A_WE
      if ROUND(warT_ewiD-pm*ut,A_ZAOKR)#0
        changed:=.t.
        alarm("WARTOóè SUMY POZYCJI NIEZGODNA Z INFORMACJ§ W NAGù‡WKU;DOKONANO KOREKTY",,,3)
        warT_ewiD:=pm*ut
      ENDIF
#endif
#ifdef A_FA
#ifdef A_NVAT
      v:=vt
      vt:=0
      aeval(was,{|x|x[2]:=round(100*x[2],0),x[3]:=x[2]*val(x[1])/100,vt+=round(x[3],0)})
      vt/=100
#endif
      if dok_zew$"UV" .and. ROUND(warT_vaT-vt,A_ZAOKR)#0
         if fakkorflag=NIL .and. i=1 .and. (tone(130,3),if(changed,1=alarm("NIEZGODNA WARTOóè PODATKU;CZY DOKONAè KOREKTY ?",{"TAK","NIE"},2,2),NIL=alarm("NIEZGODNA WARTOóè PODATKU;NIE DOKONANO KOREKTY !!!")))
             wart_vat:=vt
             for k:=1 to len(stawki)
                if val(stawki[k])#0
                   x:=ascan(was,{|x|x[1]=stawki[k]})
                   if x#0
                      x:=was[x,3]
                   endif
                   field2biN(fieldpos(D_NVAT+ltrim(stawki[k])),x)
                endif
             next k
         else
             vt:=wart_vat
             for k:=1 to len(stawki)
                if val(stawki[k])#0 .and. (n:=fieldpos(D_NVAT+ltrim(stawki[k])))#0
                   x:=ascan(was,{|x|x[1]=stawki[k]})
                   if x=0
                      if bin2D(fieldget(n))#0
                         aadd(was,{stawki[k],0,bin2D(fieldget(n))})
                      endif
                   else
                      was[x,3]:=bin2D(fieldget(n))
                   endif
                endif
             next k
         endif
      endif
      if dok_ew#"E" .and. ROUND(wartosc-wt,A_ZAOKR)#0
      if fakkorflag=NIL .and. i=dok_kop .and.changed
         WARTOSC:=wt
         #ifdef A_DF
           field2bin('d_wartosc',w)
         #endif
         //changed:=.t.
         alarm("WARTOóè SUMY POZYCJI NIEZGODNA Z INFORMACJ§ W NAGù‡WKU;DOKONANO KOREKTY",,,3)
      else
         alarm("NIEZGODNA WARTOóè SUMY POZYCJI;NIE DOKONANO KOREKTY !!!",,,3)
         wt:=WARTOSC
      endif
      endif
#ifdef A_NVAT
      aeval(was,{|x|x[2]/=100,x[3]:=round(x[3],0)/100})
#endif
#endif
      if POZYCJA#P
        changed:=.t.
        alarm("ILOóè POZYCJI NIEZGODNA Z INFORMACJ§ W NAGù‡WKU;DOKONANO KOREKTY",,,3)
        pozycja:=p
      ENDIF
   if dok_war#"-"
    if p>D_LP1
#ifdef A_GRAM
      ?? "WAGA: "+ltrim(str(gt,10,2))+" kg",spec(chr(13))
#endif
      ?? ccpi(7)+space(wp17),ccpi(5)+space(wp12),ccpi(4)+space(wp10-15)+strpic(ut,15,A_ZAOKR,"@E ")
      ?
    endif
#ifndef A_FA
#ifdef A_VAT
      if dok_zew$"UV" .and. wart_vat#0
           ?? ccpi(7)+space(wp17),ccpi(5)+space(wp12),ccpi(4)+padl("VAT +",wp10-15)+strpic(wart_vaT,15,A_ZAOKR,"@E ")
           ?  ccpi(7)+space(wp17),ccpi(5)+space(wp12),ccpi(4)+padl("=",wp10-15)+strpic(wt+wart_vaT,15,A_ZAOKR,"@E ")
           ?
      endif
#endif
#endif
   endif
      if dok_fot="&:"
      TEXT &(trim(subs(dok_fot,3)))
      else
      TEXT dok_fot
      ? kto_pisal
      endif
      ?? spec(chr(13)+chr(12))
**************
#ifdef A_FA
      endif
#endif
**************
      setprc(0,0)
  next i
  END
  UNLOCK
  message(mes)
  SET PRINT OFF
#ifdef D_HWPRN
if valtype(oprn)='O'
   oprn:Destroy()
   oprn:=NIL
endif
#endif
#ifdef A_PRINT
               x:=set(_SET_PRINTFILE,'')
               if ! set(_SET_PRINTFILE)==x .and. File(x)
               A_PRINT(x)
               endif
#else
               set printer to
#endif
  SET CONSOLE ON

RETURN
*******************************
#endif
************
