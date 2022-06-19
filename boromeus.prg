field data,nr_mag,index,ilosc,ilosc_f,wartosc,stan,stanx,wartoscx,pozycja,nr_dowodu,smb_dok,id,;
	proc_vat,data_zmian,data_roz,unlink,nazwa,cena,cena3,proc_mar3,parent,rodz_tow,new_fota,grupa,jm,gram,;
	uwagi,grupy,zapas_id,nr_faktury,data_id,przel,jm_opcja
memvar dokumenty,mag_poz,mag_biez,dzisiaj,defa

func e_edi(ko)
local a,b,c,d,e,f,g,h,i
MEMVAR r,da,wa,ck,il,nowystan,avat

 IF ko='"'
    ko:=subs(ko,2,len(ko-2))
 ENDIF
 IF !file(ko)
   return .f.
 ENDIF

 altd()

 sel('i_odczyt','i_odczyt')

 IF lower(right(ko,4))='.fak'
    a:=getlines(hb_translate(memoread(ko),'PLMAZ',))


 b:=ascan(a,{|x|c:=at('NipDostawcy',x),c<>0})

 if b=0
    return .f.
 endif

 append blank

 c:=strtran(subs(a[b+1],c,13),'-')

 sel('firmy','firm_nip')
 if dbseek( c ,.f.)
   i_odczyt->dost_odb:=firmy->numer_kol+' '+firmy->nazwa
 else
   alarm('Nie rozpoznaj© tej firmy po NIP!')
   return .f.
   //i_odczyt->dost_odb:=c
 endif

 select i_odczyt

 c:=at('SymbolFaktury',a[b])
 field->nr_faktury:=subs(a[b+1],c)

 c:=at('DataFaktury',a[b])

 field->data:=field->data_dost:=stod(strtran(subs(a[b+1],c,10),'.'))

 dbcreate(c:=defa+'odczyt'+HB_ps()+'i'+strtran(str(lastrec(),7),' ','0'),{{'ean','C',13,0},{'bloz','C',7,0},{'nazwa','C',46,0},{'jm','C',4,0},{'nr_zlec','C',30,0},{'cena','N',10,2},{'ilosc','N',10,3}})

 dbselectar(select('i_od0'))
 use (c) alias i_od0
 b+=2
 For d:=b+1 to len(a)-3

  dbappend()

  c:=at('NazwaLeku',a[b])
  field->nazwa:=subs(a[d],c,40)

  c:=at('Ilosc',a[b])
  field->ilosc:=val(subs(a[d],c,14))

  c:=at('CenaHurBrutto',a[b])
  field->cena:=val(subs(a[d],c,13))*Field->ilosc

  c:=at('DataWaznosci',a[b])
  f:=at('Seria',a[b])

  field->nr_zlec:=dtoc(stod(strtran(subs(a[d],c,10),'.')))+' '+subs(a[d],f,c-f)

  c:=at('KodKreskowy',a[b])
  field->KoD:=subs(a[d],c,13)

 next b

 use
 select i_odczyt
 use


   return .t.
 ENDIF

sel('indx_mat','indx_kod')

DO WHILE .T.

 a:=mxmlNewXML()
 a:=mxmlLoadFile(a,ko)
 if empty(a)
   EXIT
 endif

 g:=b:=mxmlfindelement(a, a, "kontrahenci",,,1 )


 if empty(b)
   EXIT
 endif

 f:=''

 while !empty(g:=mxmlfindelement(g, b, "kontrahent",,,1 ))

   c:=strtran(mxmlgetText( mxmlfindelement(g, g, "nip",,,1 ) ),'-')
   if c<>'5481517764'

 sel('firmy','firm_nip')
 if dbseek( c ,.f.)
   f:=firmy->numer_kol+' '+firmy->nazwa
 else
   e:=mxmlfindelement(g, g, "nazwa",,,1 )
   e:=mxmlGetFirstChild(e)
   f:=mxmlgetcdata(e)
   if empty(f)
     f:=''
     do while e<>NIL
       f+=mxmlgetTEXT( e )+' '
       e:=MXMLGETNEXTSIBLING(e)
     end
   else
     f:=strtran(f,']]')
   endif
   alarm('Nie rozpoznaj© tej firmy po NIP: '+c+' !;'+f)
   f:=NIL
 endif

      exit
   endif

 enddo

 if empty(f)
    exit
 endif

 d:={}
 g:=b:=mxmlfindelement(a, a, "towary",,,1 )

 while !empty(g:=mxmlfindelement(g, b, "towar",,,1 ))
   c:=mxmlfindelement(g, g, "nazwa",,,1 )
   c:=mxmlGetFirstChild(c)
   e:=mxmlgetcdata(c)
   if empty(e)
   e:=''
   do while c<>NIL
     e+=mxmlgetTEXT( c )+' '
     c:=MXMLGETNEXTSIBLING(c)
   end
   c:=mxmlfindelement(g, g, "postac",,,1 )
   if !empty(c)
     c:=mxmlGetFirstChild(c)
     do while c<>NIL
      e+=mxmlgetTEXT( c )+' '
      c:=MXMLGETNEXTSIBLING(c)
     end
   end
   c:=mxmlfindelement(g, g, "dawka",,,1 )
   if !empty(c)
     c:=mxmlGetFirstChild(c)
     do while c<>NIL
       e+=mxmlgetTEXT( c )+' '
       c:=MXMLGETNEXTSIBLING(c)
     end
   end
   c:=mxmlfindelement(g, g, "opakowanie",,,1 )
   if !empty(c)
     c:=mxmlGetFirstChild(c)
     do while c<>NIL
      e+=mxmlgetTEXT( c )+' '
      c:=MXMLGETNEXTSIBLING(c)
     end
   end
   else
     e:=strtran(e,']]')
   endif

   c:=mxmlfindelement(g, g, "ean",,,1 )
   if empty(c)
      c:=''
   else
      c:=mxmlgetTEXT( c )
   endif

   aadd(d,{mxmlgetText(mxmlfindelement(g, g, "id-towaru",,,1 )),e,mxmlgetText( mxmlfindelement(g, g, "id-towaru-ks",,,1 )),c})

 enddo

 b:=mxmlfindelement(a, a, "naglowek",,,1 )

 c:=mxmlfindelement(b, b, "nr-faktury",,,1 )
 if empty(c)
   c:=mxmlgettext(mxmlfindelement(b, b, "id-zamowienia",,,1 ))
 else
   c:=mxmlGetFirstChild(c)
   e:=mxmlgetcdata(c)
   if empty(e)
     c:=mxmlgetText(c)
   else
     c:=strtran(e,']]')
   endif
 endif
 select i_odczyt

 if dbseek(firmy->numer_kol+c)
    LOCK
 else
    append blank
 endif
 field->dost_odb:=f
 field->data:=field->data_dost:=stod(strtran(mxmlgetText(mxmlfindelement(b, b, "data-wystawienia",,,1 )),'-'))
 field->nr_faktury:=c

 c:=defa+'odczyt'+HB_ps()+'i'+strtran(str(lastrec(),7),' ','0')
 ferase(c+'.dbf')
 dbcreate(c,{{'ean','C',13,0},{'bloz','C',7,0},{'nazwa','C',46,0},{'jm','C',4,0},{'nr_zlec','C',30,0},{'cena','N',10,2},{'ilosc','N',10,3}})

 dbselectar(select('i_od0'))
 use (c) alias i_od0

 c:=b:=mxmlfindelement(b, a, "pozycje",,,1 )

 DO WHILE !empty(c:=mxmlfindelement(c, b, "pozycja",,,1 ))

  dbappend()
  e:=mxmlgetTEXT(mxmlfindelement(c, c, "id-towaru",,,1 ))
  e:=d[ascan(d,{|x|x[1]=e})]

  field->nazwa:=e[2]

  field->bloz:=e[3]

  field->ean:=e[4]

  field->ilosc:=val(mxmlgetText( mxmlfindelement(c, c, "ilosc",,,1 ) ))

  e:=mxmlfindelement(c, c, "jednostka-miary",,,1 )
  if !empty(e)
    field->jm:=mxmlgetText( e )
  endif

  e:=mxmlfindelement(c, c, "wartosc-brutto",,,1 )
  if !empty(e)
     field->cena:=val(mxmlgetText( e ))
  endif

  f:=mxmlfindelement(c, c, "seria",,,1)
  if !empty(f)
    f:=mxmlGetFirstChild(f)
    e:=mxmlgetcdata(f)
    if empty(e)
      e:=mxmlgetText(f)
    else
      e:=strtran(e,']]')
    endif
    field->nr_zlec:=dtoc(stod(strtran(mxmlgetText( mxmlfindelement(c, c, "data-waznosci",,,1 ) ),'-')))+' '+e
  endif

  f:=mxmlfindelement(c, c, "kod-kreskowy",,,1 )
  if empty(f)
    f:=field->ean
  else
    f:=mxmlgetText( f )
    field->ean:=f
  endif
  if !empty(field->ean)
    indx_mat->(ordsetfocus('indx_kod'))
    if indx_mat->(dbseek(mag_biez+f,.f.).and.' '$field->sww)
       LOCK IN INDX_MAT
       indx_mat->sww:=field->bloz
    endif
    f:=field->bloz
    indx_mat->(ordsetfocus('indx_sww'))
    if indx_mat->(dbseek(mag_biez+f,.f.).and.' '$field->ean)
       LOCK IN INDX_MAT
       indx_mat->ean:=field->ean
    endif
  endif

 ENDDO

 use
 select i_odczyt
 use

 exit

ENDDO

mxmldelete(a)

return .t.
