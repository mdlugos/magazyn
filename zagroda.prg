#require "hbmxml"

field data,nr_mag,index,ilosc,ilosc_f,wartosc,stan,stanx,wartoscx,pozycja,nr_dowodu,smb_dok,id,;
	proc_vat,data_zmian,data_roz,unlink,nazwa,cena,cena3,proc_mar3,parent,rodz_tow,new_fota,grupa,jm,gram,;
	uwagi,grupy,zapas_id,nr_faktury,data_id,przel,jm_opcja
memvar dokumenty,mag_poz,mag_biez,dzisiaj,defa
**************
func e_cat

   LOCAL tree, node, group, element, i
   LOCAL xData, _id, name, hp:='.'+HB_OsPathSeparator()


   sel('kategor','id')

   mxmlsetwrapmargin(250)

   __run('curl --user WB4JISQ37BWK0FO458X1FTPBJF35PC2B: http://zagroda.cieszyn.pl/api/categories/ > '+hp+'exp_cat.xml')

   tree:=mxmlNewXML()
   tree:=mxmlLoadFile(tree,hp+'exp_cat.xml')

   group := mxmlfindelement(tree, tree, "categories", NIL, NIL, MXML_DESCEND )
   if !empty(group)
    set order to 0
    set deleted off
    go top
    node := mxmlGetFirstChild( group )
    while !empty(node)
      name := mxmlGetElement( node )
      if name == "category"
        i:=val(mxmlElementGetAttr( node , "id" ))
	if eof()
	   append blank 
	else
	   lock
	   recall
	endif
	field->id := i
	unlock
	skip
      endif
      node :=mxmlGetNextSibling( node )
    enddo
    set deleted on
    delete rest for dbrlock()
    unlock
   endif

   mxmlDelete(tree)

   set order to tag id
   set deleted on
   go top

   while ! eof()
   
        ferase(hp+'exp_cat.xml')
	
        __run('curl --user WB4JISQ37BWK0FO458X1FTPBJF35PC2B: http://zagroda.cieszyn.pl/api/categories/'+ltrim(str(id))+' > '+hp+'exp_cat.xml')
        tree    := mxmlNewXML()
        tree:=mxmlLoadFile(tree,hp+'exp_cat.xml')
	lock
   
        group := mxmlfindelement(tree, tree, "id_parent", NIL, NIL, MXML_DESCEND )
    	element := mxmlGetFirstChild(group)
        xData:= mxmlgetcData( element )
	    field->parent:=val(xData)

        group := mxmlfindelement(group, tree, "name", NIL, NIL, MXML_DESCEND )
        group := mxmlfindelement(group, tree, "language", NIL, NIL, MXML_DESCEND )
	    element := mxmlGetFirstChild(group)
        xData:= mxmlgetcData( element )
	    xData:=left(xData,len(xData)-2)
        mxmlDelete(tree)
	    i:=recno()
        seek parent
      	if found()
    	  lock
    	  field->dzieci := .t.
     	  xData +='|'+trim(nazwa)
        end if
        go i
    	rlock()
    	field->nazwa:=xData
        skip
   end do

return .t.   
************************************
func e_edi(ko,fi)
local a,b,c,d,e,f,hp:='.'+HB_OsPathSeparator()             //getenv('HOME')
MEMVAR r,da,wa,ck,il,nowystan,avat

sel('firmy','firm_nip')

dbselectar(select('i_odczyt'))

DO WHILE .T.

 IF !file(ko)
   EXIT
 ENDIF
 a:=mxmlNewXML()
 a:=b:=mxmlLoadFile(a,ko)
 if empty(b)
   EXIT
 endif
 
 b:=mxmlfindelement(b, a, "Invoice-Header",,,1 )
 
 if empty(b)
   EXIT
 endif
 
 use (defa+'odczyt'+HB_OsPathSeparator()+'i_odczyt') 

 append blank
 
 field->data:=dzisiaj
 
 b:=mxmlfindelement(b, a, "InvoiceNumber",,,1 )
 c:=mxmlGetFirstChild(b)
 c:=mxmlgetText( c )

 field->nr_faktury:=c
 
 b:=mxmlfindelement(b, a, "InvoiceDate",,,1 )
 c:=mxmlGetFirstChild(b)
 c:=mxmlgetText( c )

 field->data_dost:=stod(strtran(c,'-'))

 b:=mxmlfindelement(b, a, "InvoicePaymentDueDate",,,1 )
 c:=mxmlGetFirstChild(b)
 c:=mxmlgetText( c )

 field->termin_p:=stod(strtran(c,'-'))

 b:=mxmlfindelement(b, a, "Seller",,,1 )
 
 b:=mxmlfindelement(b, a, "TaxID",,,1 )
 c:=mxmlGetFirstChild(b)
 c:=mxmlgetText( c )

 if empty(fi) .and. firmy->(dbseek(trim(c)))
   fi:=firmy->numer_kol
 endif

 b:=mxmlfindelement(b, a, "Name",,,1 )
 c:=mxmlGetFirstChild(b)
 c:=from_xml(mxmlgetText( c ))
 field->dost_odb:=c
 field->kontrahent:=fi

 dbcreate(c:=defa+'odczyt'+HB_OsPathSeparator()+'i'+strtran(str(lastrec(),7),' ','0'),{{'index','C',12,0},{'nazwa','C',40,0},{'cena','N',10,2},{'proc_vat','C',2,0},{'ilosc','N',10,3}})
 use (c) alias i_odczyt
 
 DO WHILE .T.
  b:=mxmlfindelement(b, a, "EAN",,,1 )
  IF EMPTY(b)
    EXIT
  ENDIF
  c:=mxmlGetFirstChild(b)

  dbappend()
  
  IF !EMPTY(c)
    c:=left(mxmlgetTEXT( c ),12)
    field->index:=c
  ENDIF
  

  b:=mxmlfindelement(b, a, "ItemDescription",,,1 )
  e:=mxmlGetFirstChild(b)
  e:=from_xml(mxmlgetTEXT( e ))
  field->nazwa:=e
    
  b:=mxmlfindelement(b, a, "InvoiceQuantity",,,1 )
  c:=mxmlGetFirstChild(b)
  c:=val(mxmlgetText( c ))
  field->ilosc:=c

  b:=mxmlfindelement(b, a, "TaxRate",,,1 )
  c:=mxmlGetFirstChild(b)
  c:=val(mxmlgetText( c ))
  field->proc_vat:=padl(c,2)
    
  b:=mxmlfindelement(b, a, "NetAmount",,,1 )
  c:=mxmlGetFirstChild(b)
  c:=val(mxmlgetText( c ))
  field->cena:=c
  
 ENDDO
 
 close

 mxmldelete(a)

 exit 

ENDDO

return ko
************************************
func e_ord(ko)  //odczyt z e-sklepu zamiowienia -> paragon
local stat,a,b,c,d,e,f,hp:='.'+HB_OsPathSeparator() //getenv('HOME')
MEMVAR r,da,wa,ck,il,nowystan
PRIVATE r:=max(1,ascan(dokumenty[MAG_POZ],'PA')),da,wa,ck,il,nowystan


stat:=push_stat()

sel('e_prod','id')

sel('stany','stan_mag')

sel('indx_mat','indx_num')
 
sel('main','main_nrk')

sel('dm',1)


DO WHILE .T.

 ferase(hp+'exp_ord.xml')
 
 __run('curl --user WB4JISQ37BWK0FO458X1FTPBJF35PC2B: http://zagroda.cieszyn.pl/api/orders/'+alltrim(ko)+' > '+hp+'exp_ord.xml')
 IF !file(hp+'exp_ord.xml')
   EXIT
 ENDIF
 a:=mxmlNewXML()
 a:=b:=mxmlLoadFile(a,hp+'exp_ord.xml')
 if empty(b)
   EXIT
 endif
 
 b:=mxmlfindelement(b, a, "order",,,1 )
 
 if empty(b)
   EXIT
 endif
 
 select dm

 dbseek(mag_biez+'PAZZZZZ')
 
 dbskip(-1)
 d:=nr_dowodu
 IF ! (val(pozycja)=0 .and. rlock())
    d:=str(val(d)+1,5)
    dbappend()
    field->nr_mag:=mag_biez
    field->smb_dow:='PA'
    field->nr_dowodu:=d
    dm->pozycja:=field->pozycja:=str(0,len(pozycja))
 endif
 field->data:=field->data_dost:=dzisiaj
 field->nr_faktury:='e:'+ko
 field->kto_pisal:=chr(255)+chr(0)

 b:=mxmlfindelement(b, a, "id_customer",,,1 )
 c:=mxmlGetFirstChild(b)
 c:=mxmlgetcData( c )
 field->kontrahent:='@'+str(val(c)%10000,4)
 field->dost_odb:='@'+str(val(c))
 
 ferase(hp+'exp_cus.xml')
 __run('curl --user WB4JISQ37BWK0FO458X1FTPBJF35PC2B: http://zagroda.cieszyn.pl/api/customers/'+alltrim(str(val(c)))+' > '+hp+'exp_cus.xml')
 IF file(hp+'exp_cus.xml')
   c:=getlines(memoread(hp+'exp_cus.xml'))
   d:=strtran(c[13]+c[14],chr(9)+'<lastname><![CDATA[')
   d:=strtran(d,chr(9)+'<firstname><![CDATA[')
   d:=strtran(d,']]></lastname>',' ')
   d:=strtran(d,']]></firstname>')
   field->dost_odb:='e-sklep '+trim(field->dost_odb)+' '+hb_translate(d,'UTF8',)
   field->nazwisko:=str(val(subs(c[35],21,6)),6)
 ENDIF
 
 b:=mxmlfindelement(b, a, "current_state",,,1 )
 c:=mxmlGetFirstChild(b)
 c:=hb_translate(mxmlgetcData( c ),'UTF8',)
 field->transport:=str(val(c),2)
 
 b:=mxmlfindelement(b, a, "payment",,,1 )
 c:=mxmlGetFirstChild(b)
 c:=hb_translate(mxmlgetcData( c ),'UTF8',)
 field->nr_czeku:=left(c,len(c)-2)

 b:=mxmlfindelement(b, a, "total_discounts",,,1 )
 c:=mxmlGetFirstChild(b)
 c:=mxmlgetcData( c )
 f:=c

 b:=mxmlfindelement(b, a, "total_paid",,,1 )
 c:=mxmlGetFirstChild(b)
 c:=mxmlgetcData( c )
 field->wartosc:=field->przelewem:=val(c)

 b:=mxmlfindelement(b, a, "total_shipping",,,1 )
 c:=mxmlGetFirstChild(b)
 c:=mxmlgetcData( c )

 __run('curl --user WB4JISQ37BWK0FO458X1FTPBJF35PC2B: http://zagroda.cieszyn.pl/api/customers/'+alltrim(ko)+' > '+hp+'exp_ord.xml')
 IF !file(hp+'exp_ord.xml')
   EXIT
 ENDIF

 select main
 
 IF !f==c
   indx_mat->(dbseek('000936 '))
   dbappend()
   field->nr_mag:=mag_biez
   field->smb_dow:='PA'
   field->nr_dowodu:=dm->nr_dowodu
   dm->pozycja:=field->pozycja:=str(1,len(pozycja))
   field->data:=dm->data
   field->cena:=val(c)
   field->index:=indx_mat->index
   field->proc_vat:=indx_mat->proc_vat
   field->ilosc_f:=-1
   
   nowystan:= .not.stany->(dbseek(mag_biez+main->index,.f.))
   if nowystan .or. stany->(reclock())
     da:=data
     wa:=ck:=0
     il:=ilosc_f
     GETCK(.t.)
     e:=ROUND(IL*CK,2)
     DM->wart_ewid:=Round(DM->wart_ewid+e-wartosc,A_ZAOKR)
     STANY->STAN:=Round(STANY->stan+ILOSC_F-ILOSC,3)
     STANY->WARTOSC:=round(STANY->wartosc+e-WARTOSC,A_ZAOKR)
     STANY->STANx:=Round(STANY->stanx+ILOSC_F-ILOSC,3)
     STANY->WARTOSCx:=round(STANY->wartoscx+e-WARTOSC,A_ZAOKR)
     MAIN->ILOSC:=ILOSC_F
     MAIN->WARTOSC:=e
     STANY->DATA_ROZ:=MAX(DA,STANY->DATA_ROZ)
     STANY->DATA_ZMIAN:=STANY->(MAX(DATA_ZMIAN,DATA_ROZ))
     STANY->unlink:=.t.
     STANY->(dbunlock())
   endif  
 ENDIF

 DO WHILE .T.
  b:=mxmlfindelement(b, a, "product_id",,,1 )
  IF EMPTY(b)
    EXIT
  ENDIF
  c:=mxmlGetFirstChild(b)
  c:=int(val(mxmlgetcData( c )))
  if ! e_prod->(dbseek(c,.f.)) .or. ! indx_mat->(dbseek(e_prod->index,.f.))
    exit
  endif
  dbappend()
  field->nr_mag:=mag_biez
  field->smb_dow:='PA'
  field->nr_dowodu:=dm->nr_dowodu
  dm->pozycja:=field->pozycja:=str(val(dm->pozycja)+1,len(pozycja))
  field->data:=dm->data
  field->index:=indx_mat->index
  field->proc_vat:=indx_mat->proc_vat

  b:=mxmlfindelement(b, a, "product_quantity",,,1 )
  c:=mxmlGetFirstChild(b)
  il:=-val(mxmlgetcData( c ))

  b:=mxmlfindelement(b, a, "unit_price_tax_incl",,,1 )
  c:=mxmlGetFirstChild(b)
  c:=val(mxmlgetcData( c ))
  
  if e_prod->(przel>1 .and. UpP(jm)<>jm_opcja .and. jm_opcja='SZT')
     il*=e_prod->przel
     c/=e_prod->przel
  endif
  
  field->ilosc_f:=il
  field->cena:=c
  
  nowystan:= .not.stany->(dbseek(mag_biez+main->index,.f.))
  if nowystan .or. stany->(reclock())
  
  da:=data
  wa:=ck:=0
  GETCK(.t.)
  e:=ROUND(IL*CK,2)
     DM->wart_ewid:=Round(DM->wart_ewid+e-wartosc,A_ZAOKR)
     STANY->STAN:=Round(STANY->stan+ILOSC_F-ILOSC,3)
     STANY->WARTOSC:=round(STANY->wartosc+e-WARTOSC,A_ZAOKR)
     STANY->STANx:=Round(STANY->stanx+ILOSC_F-ILOSC,3)
     STANY->WARTOSCx:=round(STANY->wartoscx+e-WARTOSC,A_ZAOKR)
  MAIN->ILOSC:=ILOSC_F
  MAIN->WARTOSC:=e
  STANY->DATA_ROZ:=MAX(DA,STANY->DATA_ROZ)
  STANY->DATA_ZMIAN:=STANY->(MAX(DATA_ZMIAN,DATA_ROZ))
  STANY->unlink:=.t.
  IF indx_mat->zapas_min>0 .and. indx_mat->zaznacz<>2 .and. STANY->stan<indx_mat->zapas_min .and. indx_mat->(dbrlock())
     indx_mat->zaznacz:=2
     indx_mat->(dbunlock())
  ENDIF
  
  STANY->(dbunlock())

  endif
 ENDDO

 mxmldelete(a)
 ko:=ltrim(str(val(ko)+1,10))
ENDDO

UNLOCK IN DM
UNLOCK IN MAIN
UNLOCK IN STANY

pop_norec(stat)

return ko
************************************
proc e_stock(all)
local tree, group, element, node,hp:='.'+HB_OsPathSeparator() //getenv('HOME')
LOCAL xData, _id, name, i, stat
//LOCAL curl

stat:= push_stat()

sel('indx_mat','indx_num')
sel('stany','stan_chg')
set relation to index into indx_mat

if !empty(all)
  set order to tag stan_mag
endif

   set filter to indx_mat->zapas_id<>0 .and. indx_mat->id<>0 .and. 'S'$indx_mat->rodz_tow

   seek mag_biez
  
   mxmlsetwrapmargin(250)

   tree    := mxmlNewXML()

   group   := mxmlNewElement( tree, 'prestashop' )
   mxmlElementSetAttr( group, "xmlns:xlink", "http://www.w3.org/1999/xlink" )
     

do while nr_mag=mag_biez

   element := mxmlNewElement( group, "stock_available")
   
   node := mxmlNewElement( element, "id" )
   mxmlNewText( node,, ltrim(str(indx_mat->zapas_id)))
   
   node := mxmlNewElement( element, "id_product" )
   mxmlElementSetAttr( node, "xlink:href", "http://zagroda.cieszyn.pl/api/products/"+ltrim(str(indx_mat->id)) )
   mxmlNewText( node,, ltrim(str(indx_mat->id))+hb_eol())
   
   node := mxmlNewElement( element, "id_product_attribute" )
   mxmlNewText( node,, "0")

   node := mxmlNewElement( element, "id_shop" )
   mxmlElementSetAttr( node, "xlink:href", "http://zagroda.cieszyn.pl/api/shops/1" )
   mxmlNewText( node,, "1")

   node := mxmlNewElement( element, "id_shop_group" )
   mxmlNewText( node,, "0")
   
   node := mxmlNewElement( element, "quantity" )
   mxmlNewText( node,, ltrim(str(stan,10,0)))
   
   node := mxmlNewElement( element, "depends_on_stock" )
   mxmlNewText( node,, "0")
   
   node := mxmlNewElement( element, "out_of_stock" )
   mxmlNewText( node,, IF('Z'$indx_mat->rodz_tow,"1","2"))

   if dbrlock() 
      unlink := .f.
      dbunlock()
   endif

   skip
   
end do

set order to tag stan_mag

set filter to

if !empty(element)
   ferase(hp+"exp_sto.xml")
   ferase(hp+"exp_rep.xml")
   mxmlSaveFile( tree, hp+"exp_sto.xml", @wscb() )
   mxmlDelete(tree)
   
*
   __run('curl --user WB4JISQ37BWK0FO458X1FTPBJF35PC2B: -X PUT -d @'+hp+'exp_sto.xml http://zagroda.cieszyn.pl/api/stock_availables/ > '+hp+'exp_rep.xml')
else
   mxmlDelete(tree)
endif
   
pop_stat(stat)   

return
***********************
func e_exp()

   LOCAL tree, node, group, element, i, j, k, ap
   LOCAL xData, _id, name, fota_id
   LOCAL stawki_zby:={'23',' 8',' 7',' 5',' 4',' 0'}
   local hp:='.'+HB_OsPathSeparator()             //getenv('HOME')
   local stat


   ferase(hp+'exp_pro.xml')

   __run('curl --user WB4JISQ37BWK0FO458X1FTPBJF35PC2B: http://zagroda.cieszyn.pl/api/products/ > '+hp+'exp_pro.xml')

   ap:=getlines(memoread(hp+'exp_pro.xml'))
   
   if len(ap)<5 .or. ap[3]<>'<product'
      return .f.
   endif 
   
   stat:=push_stat()   
   
   sel('stany','stan_ind')
   sel('indx_mat','indx_id')

   ? 'Kasowanie'
   ?
   ?

   for i:=len(ap) to 1 step -1
     _id:=int(val(subs(ap[i],14)))
     if _id=0
        adel(ap,i)
        asize(ap,len(ap)-1)
     else
        ap[i]:=_id
	    if !dbseek(_id,.f.) .or. ( !'S'$indx_mat->rodz_tow ) 
           ?? _id,nazwa
           ?
           __run('curl --user WB4JISQ37BWK0FO458X1FTPBJF35PC2B: -X DELETE http://zagroda.cieszyn.pl/api/products/'+ltrim(str(_id))+'/')
           adel(ap,i)
           asize(ap,len(ap)-1)
        endif
     endif
   next

set order to tag 'indx_num' // zmiana id moze wywalic z kolejnosci

set filter to !empty(id)
go top

do while !eof()
  i:=ascan(ap,id)
  if i=0 .and. reclock()
     id:=0
     zapas_id:=0
     new_fota:=''
     rodz_tow:=strtran(strtran(rodz_tow,'X'),'S')+'X'
     dbunlock()
  endif
  skip
enddo

set filter to

sel('e_prod','id')

set order to 0

go top

//i:=.f.

do while !eof()
  if ascan(ap,id)=0 .and. reclock()
     DELETE
     dbunlock()
  endif
  skip
enddo

set order to tag id

select indx_mat

set filter to 'S' $ rodz_tow .and. empty(id)
go top
   mxmlsetwrapmargin(250)

if !eof()
****
? 'Nowe produkty:'
?
?
do while !eof()

   e_prod->(dbgoto(0))

   ?? nazwa

   tree    := mxmlNewXML()

   group   := mxmlNewElement( tree, 'prestashop' )
   mxmlElementSetAttr( group, "xmlns:xlink", "http://www.w3.org/1999/xlink" )


   element := mxmlNewElement( group, "product" )

   do_aktual(element)
/*   
   node := mxmlNewElement( element, "price" )
   mxmlNewText( node,, ltrim(str(cena)) )

   node := mxmlNewElement( element, "ean13" )
   mxmlNewText( node,, trim(index) )
   
   node := mxmlNewElement( element, "link_rewrite" )
   node := mxmlNewElement( node, "language" )
   mxmlElementSetAttr( node, "id", "1" )
   mxmlElementSetAttr( node, "xlink:href", "http://zagroda.cieszyn.pl/api/languages/1" )
   mxmlNewText( node,, make_url(trim(nazwa)))

   node := mxmlNewElement( element, "name" )
   node := mxmlNewElement( node, "language" )
   mxmlElementSetAttr( node, "id", "1" )
   mxmlElementSetAttr( node, "xlink:href", "http://zagroda.cieszyn.pl/api/languages/1" )
   mxmlNewText( node,, make_url(trim(nazwa)))
*/   
   mxmlSaveFile( tree, hp+"exp_prod.xml", @wscb() )
   
   ferase(hp+"exp_rep2.xml")

   __run('curl --user WB4JISQ37BWK0FO458X1FTPBJF35PC2B: -X POST -d @'+hp+'exp_prod.xml http://zagroda.cieszyn.pl/api/products/ > '+hp+'exp_rep2.xml')

   mxmlDelete(tree)

   tree    := mxmlNewXML()
   tree    := mxmlLoadFile(tree,hp+'exp_rep2.xml')
   
   node   := mxmlfindelement(tree, tree, "id", NIL, NIL, MXML_DESCEND )

   LOCK

   if empty(node)
        i:=setcolor('R+')
        ?? ' - ERROR'
        setcolor(i)
        field->rodz_tow:=strtran(field->rodz_tow,'S',"X")
        field->zaznacz:=2
   else

        i:=setcolor('G+')
        ?? ' - OK'
        setcolor(i)
        element:=mxmlGetFirstChild( node )
        xData:= mxmlgetcdata( element )
        field->id:=val(xData)
	field->data_id:=date()
	
        node   := mxmlfindelement(node, tree,'stock_available', NIL, NIL, MXML_DESCEND )
        element := mxmlfindelement(node, tree, "id", NIL, NIL, MXML_DESCEND )
        element := mxmlGetFirstChild( element )
        xData:= mxmlgetcdata( element )
        field->zapas_id:=val(xData)
	field->new_fota:=''
        xData:=e_prod->(dbstruct())
	e_prod->(dbappend())
        for i:=1 to len(xData)
           e_prod->(fieldput(i,indx_mat->(fieldget(fieldpos(xData[i,1])))))
        next i
        e_prod->(dbunlock())
        select stany
        if dbseek(INDX_MAT->index+mag_biez) .and.  rlock()
          field->unlink:=.t.
          dbunlock()
        endif
        select indx_mat
   endif

   mxmlDelete(tree)
   ?
   
   skip
enddo

endif
************************************


//select indx_mat

/*********
set order to tag indx_id
set filter to 'S' $ rodz_tow .and. zapas_id<id
go top

?
? 'zapas ID'
?
?

do while !eof()
  ?? nazwa,id,''
  LOCK
  ferase(hp+"exp_rep3.xml")
  __run('curl --user WB4JISQ37BWK0FO458X1FTPBJF35PC2B: http://zagroda.cieszyn.pl/api/products/'+ltrim(str(id))+'/ > '+hp+'exp_rep3.xml')

   tree    := mxmlNewXML()
   tree    := mxmlLoadFile(tree,hp+'exp_rep3.xml')
   
   node   := mxmlfindelement(tree, tree,'stock_available', NIL, NIL, MXML_DESCEND )
   element := mxmlfindelement(node, tree, "id", NIL, NIL, MXML_DESCEND )
   element := mxmlGetFirstChild( element )
   xData:= mxmlgetcdata( element )
  ?? field->zapas_id:=val(xData)
   mxmlDelete(tree)
  dbunlock()
  ?
  skip
enddo

**************************************/
set order to tag indx_id
set relation to id into e_prod
set filter to 'S' $ rodz_tow
go top


?
? 'Aktualizacja:'
?
?
do while !eof()
    fota_id := NIL
    xData := directory('/srv/etykiety/kreski/'+left(index,4)+'/'+trim(subs(index,5))+"*.jpg")
    _id := new_fota
    asort(xData,,,{|x,y|x[1]>y[1]})
    for i:=1 to len(xData)
       if dtos(xData[i,3])+' '+xData[i,4] > new_fota
         ?? nazwa
         ? 'fotka',xdata[i,1]
         __run("curl --user WB4JISQ37BWK0FO458X1FTPBJF35PC2B: -F 'image=@/srv/etykiety/kreski/"+left(index,4)+"/"+xData[i,1]+"' http://zagroda.cieszyn.pl/api/images/products/"+ltrim(str(id))+' > '+hp+'exp_fot.xml')
*
         if xData[i,1]=trim(subs(index,5))+'.'
             fota_id:=getlines(memoread(hp+'exp_fot.xml'))
             if len(fota_id)>9
               k:=setcolor('G+')
               ?? ' - OK'
               setcolor(k)
               fota_id:=val(subs(fota_id[4],14,10))
               if fota_id<>0
                  fota_id:=ltrim(str(fota_id))
               else
                  fota_id:=NIL
               endif
             else
               k:=setcolor('R+')
               ?? ' - ERROR'
               setcolor(k)
               fota_id:=NIL
             endif
         endif
*
         if dtos(xData[i,3])+' '+xData[i,4] > _id
           _id:= dtos(xData[i,3])+' '+xData[i,4] 
         endif
         ?
       endif
    next i
     if _id > new_fota
       rlock()
       field->new_fota:=_id
     endif
*********
   xData:=e_prod->(dbstruct())
   _id:=e_prod->(eof())
   if _id
      //e_prod->(dbappend())
   else   
     for i:=1 to len(xData)
       _id := ! e_prod->(fieldget(i)) == fieldget(fieldpos(xData[i,1]))
       if _id
         //e_prod->(dbrlock())
         exit
       endif
     next i
   endif
   
   if _id
   else  //if empty(fota_id)
     skip
     loop
   endif
************
   
   ?? nazwa
   
   tree    := mxmlNewXML()
   group   := mxmlNewElement( tree, 'prestashop' )
   element := nil
   mxmlElementSetAttr( group, "xmlns:xlink", "http://www.w3.org/1999/xlink" )


*************
   element := mxmlNewElement( group, "product" )
   
   node := mxmlNewElement( element, "id" )
   mxmlNewText(node,,ltrim(str(id)))
*********************   
   do_aktual(element)
**********************
   if !empty(fota_id)
     node := mxmlNewElement( element, "id_default_image" )
     mxmlElementSetAttr( node, "xlink:href", "http://zagroda.cieszyn.pl/api/images/products/"+ltrim(str(id))+'/'+fota_id)
     mxmlElementSetAttr( node, "not_filterable", "true")
     mxmlNewText( node,, fota_id )
   endif
**********************   
   ferase(hp+"exp_pro1.xml")
   mxmlSaveFile( tree, hp+"exp_pro1.xml", @wscb() )
   mxmlDelete(tree)

   ferase(hp+"exp_rep1.xml")
   
   __run('curl --user WB4JISQ37BWK0FO458X1FTPBJF35PC2B: -X PUT -d @'+hp+'exp_pro1.xml http://zagroda.cieszyn.pl/api/products/ > '+hp+'exp_rep1.xml')

   if '<errors>'$memoread(hp+'exp_rep1.xml')
     k:=setcolor('R+')
     ?? ' - ERROR'
     setcolor(k)
     LOCK
     field->zaznacz:=2
   else
     k:=setcolor('G+')
     ?? ' - OK'
     setcolor(k)
     if field->zaznacz=2
        LOCK
        field->zaznacz:=0
     endif
     if e_prod->(eof())
        LOCK
     	field->data_id:=date()
        e_prod->(dbappend())
     else
        e_prod->(reclock())
     endif    
     xData:=e_prod->(dbstruct())
     for i:=1 to len(xData)
       e_prod->(fieldput(i,indx_mat->(fieldget(fieldpos(xData[i,1])))))
     next i
     e_prod->(dbrunlock())
   endif
   
   UNLOCK
   
   ?

   skip

enddo

set relation to
set filter to

pop_stat(stat)

return .t.
************
stat proc do_aktual(element)
local node,i,xdata
********* 
   //node := mxmlNewElement( element, "id_tax_rules_group")
   //_id:=str(ascan(stawki_zby,proc_vat),1)
   //mxmlElementSetAttr( node, "xlink:href", "http://zagroda.cieszyn.pl/api/tax_rule_groups/"+_id)
   //mxmlNewText( node,, _id )
*******
   node := mxmlNewElement( element, "ean13" )
   mxmlNewText( node,, trim(index) )
*****
if przel>1 .and. UpP(jm)<>jm_opcja .and. jm_opcja='SZT'
   node := mxmlNewElement( element, "weight" )
   mxmlNewText( node,, ltrim(str(przel*if(jm='kg',1,gram/1000))))
******   
   node := mxmlNewElement( element, "price" )
   //mxmlNewText( node,, ltrim(str(if(proc_mar3<>0,field->cena3,cena)*100/(100+val(proc_vat)))) )
   mxmlNewText( node,, ltrim(str(przel*if(proc_mar3<>0,field->cena3,cena))) )
*****
   node := mxmlNewElement( element, "unity" )
   mxmlNewText( node,, 'szt.')

else
   node := mxmlNewElement( element, "weight" )
   mxmlNewText( node,, ltrim(str(if(jm='kg',1,gram/1000))))
******   
   node := mxmlNewElement( element, "price" )
   //mxmlNewText( node,, ltrim(str(if(proc_mar3<>0,field->cena3,cena)*100/(100+val(proc_vat)))) )
   mxmlNewText( node,, ltrim(str(if(proc_mar3<>0,field->cena3,cena))) )
*****
   node := mxmlNewElement( element, "unity" )
   mxmlNewText( node,, trim(jm) )
   
endif   
*********
//   node := mxmlNewElement( element, "unit_price_ratio" )
//   mxmlNewText( node,, '1' )
*****
   node := mxmlNewElement( element, "active" )
   mxmlNewText( node,, '1')
******   
   node := mxmlNewElement( element, "available_for_order" )
   mxmlNewText( node,, '1')
*******   
   node := mxmlNewElement( element, "show_price" )
   mxmlNewText( node,, '1')
*******
//   node := mxmlNewElement( element, "date_add" )
*******
   node := mxmlNewElement( element, "link_rewrite" )
   node := mxmlNewElement( node, "language" )
   mxmlElementSetAttr( node, "id", "1" )
   mxmlElementSetAttr( node, "xlink:href", "http://zagroda.cieszyn.pl/api/languages/1" )
   mxmlNewText( node,, make_url(trim(nazwa)))
*******
   node := mxmlNewElement( element, "name" )
   node := mxmlNewElement( node, "language" )
   mxmlElementSetAttr( node, "id", "1" )
   mxmlElementSetAttr( node, "xlink:href", "http://zagroda.cieszyn.pl/api/languages/1" )

   xData:=trim(uwagi)
   if xData='{'
       i:=at('}',xData)
       mxmlNewText( node,,to_xml(subs(xData,2,i-2)))
       xData:=subs(xData,i+1)
   else  
       mxmlNewText( node,,to_xml(trim(nazwa)))
   endif
*******  
   if !empty(xData)
          
     node := mxmlNewElement( element, "description" )
     node := mxmlNewElement( node, "language" )
     mxmlElementSetAttr( node, "id", "1" )
     mxmlElementSetAttr( node, "xlink:href", "http://zagroda.cieszyn.pl/api/languages/1" )
     mxmlNewText( node,, to_xml(xData) )

     node := mxmlNewElement( element, "description_short" )
     node := mxmlNewElement( node, "language" )
     mxmlElementSetAttr( node, "id", "1" )
     mxmlElementSetAttr( node, "xlink:href", "http://zagroda.cieszyn.pl/api/languages/1" )

     i:=LEn(xData)
     DO While i>300
       i:=Rat('.',left(xData,i-1))
     enddo
     if i=0
        i:=min(200,len(xData))
     endif
     
     mxmlNewText( node,, to_xml(left(xData,i)))
     
   endif

   node := mxmlNewElement( element, "id_category_default" )
   mxmlElementSetAttr( node, "xlink:href", "http://zagroda.cieszyn.pl/api/categories/"+ltrim(str(max(1,grupa))) )
   mxmlNewText( node,, ltrim(str(max(1,grupa))) )
   
   xData:=getlines(ltrim(str(max(1,indx_mat->grupa)))+';'+trim(grupy),';')
   
     node := mxmlNewElement( element, "associations" )
     element := mxmlNewElement( node, "categories" )
     mxmlElementSetAttr( element, "node_type", "category" )
     
   for i:=1 to len(xData)
   
     node := mxmlNewElement( element, "category" )
     mxmlElementSetAttr( node, "xlink:href", "http://zagroda.cieszyn.pl/api/categories/"+alltrim(xData[i]))
     node := mxmlNewElement( node, "id" )
     mxmlNewText( node,, alltrim(xData[i]) )
     
   next i
return
************
func make_url(s)
local i,r:='',c

local f:= "CueaauccleOoiZACELlooLlSsOUTtLxcaiouAaZzEe-zCs_______AAES____Zz_______Aa________dDDEdNIIe____TU_OBONnnSsRUrUyYt____________uRr__"

for i:=1 to len(s)
  c:=subs(s,i,1)
  if asc(c)>=128
     r+=subs(f,asc(c)-127,1)
  elseif isalpha(c) .or. isdigit(c) .and. ! c$',.+'
     r+=c
  else
     r+='-'
  endif
next i
return r
***********************
stat FUNC wscb( node, where )

   LOCAL parent        /* Parent node */
   LOCAL nLevel := -1  /* Indentation level */
   LOCAL name          /* Name of element */

   name := mxmlGetElement( node )

      parent := mxmlGetParent( node )
      DO WHILE ! Empty( parent )
         nLevel++
         parent := mxmlGetParent( parent )
      ENDDO

      IF nLevel > 8
         nLevel := 8
      ELSEIF nLevel < 0
         nLevel := 0
      ENDIF
   

   IF Left( name, 4 ) == "?xml"
      IF where == MXML_WS_AFTER_OPEN
         RETURN hb_eol()
      ELSE
         RETURN NIL
      ENDIF

   ELSEIF where == MXML_WS_BEFORE_OPEN 

      RETURN Replicate( ' ', nLevel )
****
   ELSEIF where == MXML_WS_AFTER_CLOSE 
      RETURN hb_eol()
*****
   ELSEIF where == MXML_WS_AFTER_OPEN .AND. Empty( mxmlGetFirstChild( node ) )
      RETURN hb_eol()
   ENDIF

   RETURN NIL /* Return NIL for no added whitespace... */
*******
func to_xml(x)
local i,y:='',z,j
    for i:=1 To len(x)
      z:=subs(x,i)
/******      
      if z='<'
         j:=at('>',z)
	 if j=0
	    z:='&lt;'
	 else
	    z:=left(z,j)
	    i+=j
	 endif
      elseif z='>'
         z:='&gt;'
      elseif z='&'
         z:='&amp;'
      elseif z="'"
         z:='&apos;'
      elseif z='"'
         z:='&quot;'
********/	 
      if z=chr(13)+chr(10)
         ++i
         z:='<br>'
      elseif z=chr(13) .or. z=chr(10)
         z:='<br>'
      else 
         z:=left(z,1)
      endif
      y+=z
    next i  
return y // do UTF8 samo sie robi
*******
func from_xml(x)
    x:=strtran(x,'&lt;','<')
    x:=strtran(x,'&gt;','>')
    x:=strtran(x,'&apos;',"'")
    x:=strtran(x,'&quot;','"')
    x:=strtran(x,'&amp;','&')
return hb_translate(x,'UTF8',)
