#require "hbmxml"
EXTERNAL hb_jsondecode,hb_jsonencode,hb_hgetdef,hb_hautoadd,HB_BASE64ENCODE
#include "dbinfo.ch"
#define A_SHOPER

field data,nr_mag,index,ilosc,ilosc_f,stan,pozycja,nr_dowodu,smb_dow,id,wartosc,;
	proc_vat,data_zmian,data_roz,unlink,nazwa,cena,cena3,proc_mar3,parent,rodz_tow,new_fota,grupa,jm,gram,;
	uwagi,grupy,zapas_id,nr_faktury,data_id,przel,jm_opcja,stanx,wartoscx,sold,stock

memvar dokumenty,mag_poz,mag_biez,defa

func mdgettxt(n)
local c:=''
if !EMPTY(n)
   n:=mxmlgetFirstChild(n)
   c:=mxmlGetText(n)
   do while !empty( n :=mxmlGetNextSibling( n ) )
     c+=' '+mxmlGetText(n)
   enddo
endif
return c
*******
func e_edi(ko,fi)
local a,b,c,d,e,f,g,h,i,hp:='.'+HB_OsPathSeparator()             //getenv('HOME')
MEMVAR r,da,wa,ck,il,nowystan,avat
PRIVATE da

#ifdef A_LAN
DatY->(dbgoto(1))
#endif
      da:=max(DatE(),max(DatY->d_z_mies1,DatY->data_gran)+1)
      do while YEAR(da)>YEAR(DatY->D_Z_rok+1)
         da-=day(da)
      enddo


sel('firmy','firm_nip')

dbselectar(select('i_odczyt'))

DO WHILE file(ko)


 IF lower(right(ko,4))='.rtf'
    altd()
    a:=getlines(memoread(ko))
  
    b:=ascan(a,'     NIP PL')

    if empty(b)
      EXIT
    endif
    c:=subs(a[b],13,13)

    if empty(fi)
     if firmy->(dbseek(trim(c),.f.))
       fi:=firmy->numer_kol
     else
       EXIT
     endif
    endif

    use (defa+'odczyt'+HB_OsPathSeparator()+'i_odczyt')

    append blank

    field->dost_odb:=firmy->nazwa
    field->kontrahent:=fi

    field->data:=da

    b:=ascan(a,{|x|c:=at(' FAKTURA ',x),c<>0})
    c:=subs(a[b],c+9)

    d:=at(' ',c)
    field->nr_faktury:=left(c,d-1)

    Field->data_dost:=hb_ctod(subs(c,8+at(' z dnia ',c)),'YYYY.MM.DD')

    b:=ascan(a,{|x|c:=at("Termin p",x),c<>0})

    field->termin_p:=HB_ctod(subs(a[b],c+19,10),'YYYY.MM.DD')

    dbcreate(c:=defa+'odczyt'+HB_OsPathSeparator()+'i'+strtran(str(lastrec(),7),' ','0'),{{'index','C',12,0},{'nazwa','C',40,0},{'cena','N',10,2},{'proc_vat','C',2,0},{'ilosc','N',10,3}})
    use (c) alias i_odczyt
 
    b:=ascan(a,{|x|" Nazwa towaru lub us"$x},ascan(a,{|x|"PODSTAWA KOREKTY:"$x}))

    DO WHILE !empty( b:=ascan(a,"     |",b+1 ))
      if val(subs(a[b],7))=0
        LOOP
      endif
      c:=getlines(subs(a[b],44),'|')
      dbappend()


  field->index:=alltrim(c[2])
    
  field->ilosc:=val(strtran(c[4],',','.'))

  field->proc_vat:=str(val(c[7]),2)
    
  field->cena:=val(strtran(c[6],',','.'))

  ++b
  c:=subs(a[b],12)
  c:=left(c,at('|',c)-1)
  
  for i:=len(c)-3 to 1 STEP -1
    d:=subs(c,i,4) 
    if d="\'"
       c:=stuff(c,i,4,HB_TRANSLATE(chr(HB_HEXTONUM(subs(d,3))),"PLWIN",))
    endif
  next i

  field->nazwa:=c

    ENDDO

    close

    EXIT
 ENDIF

 a:=mxmlNewXML()
 a:=mxmlLoadFile(a,ko)
 if empty(a)
   EXIT
 endif
 
 b:=mxmlfindelement(a, a, "Seller" ,,,1)
 c:=mxmlfindelement(b, b, "TaxID" ,,,1)
 c:=mxmlgetText( c )
 
 if empty(fi)
   if firmy->(dbseek(trim(c),.f.))
      fi:=firmy->numer_kol
   else
      EXIT
   endif
 endif

 use (defa+'odczyt'+HB_OsPathSeparator()+'i_odczyt') 

 append blank

 c:=mxmlfindelement(b, b, "Name",,,1 ) 
 c:=mdgetTxt( c )
 field->dost_odb:=c
 field->kontrahent:=fi

 b:=mxmlfindelement(a, a, "Invoice-Header",,,1 )
 
 if empty(b)
   EXIT
 endif

 field->data:=da
 
 field->nr_faktury:=mdgetTxt( mxmlfindelement(b, b, "InvoiceNumber" ,,,1) )
 
 field->data_dost:=stod(strtran(mxmlgetText(mxmlfindelement(b, b, "InvoiceDate" ,,,1) ),'-'))

 field->termin_p:=stod(strtran(mxmlgetText( mxmlfindelement(b, b, "InvoicePaymentDueDate" ,,,1) ),'-'))


 dbcreate(c:=defa+'odczyt'+HB_OsPathSeparator()+'i'+strtran(str(lastrec(),7),' ','0'),{{'index','C',12,0},{'nazwa','C',40,0},{'cena','N',10,2},{'proc_vat','C',2,0},{'ilosc','N',10,3}})
 use (c) alias i_odczyt
 
 c:=b:=mxmlfindelement(a, a, "Invoice-Lines",,,1 )

 DO WHILE !empty( c:=mxmlfindelement(c, b, "Line",,,1 ))

  dbappend()
  
  field->index:=mxmlgetTEXT( mxmlfindelement(c, c, "EAN",,,1 ) )
  
  field->nazwa:=mdgetTxt( mxmlfindelement(c, c, "ItemDescription",,,1 ) )
    
  field->ilosc:=val(mxmlgetText( mxmlfindelement(c, c, "InvoiceQuantity",,,1 ) ))

  field->proc_vat:=padl(val(mxmlgetText( mxmlfindelement(c, c, "TaxRate",,,1 ) )),2)
    
  field->cena:=val(mxmlgetText( mxmlfindelement(c, c, "NetAmount",,,1 ) ))
  
 ENDDO
 
 close

 mxmldelete(a)

 exit 

ENDDO

return ko
************************************
func e_ord(ko)  //odczyt z e-sklepu zamowienia -> paragon
local stat,a,b,c,d,e,f,i,pg,pl,og,ol,dt,l1,l2,hp:='.'+HB_OsPathSeparator() //getenv('HOME')
MEMVAR r,da,wa,ck,il,nowystan
PRIVATE r:=max(1,ascan(dokumenty[MAG_POZ],'PA')),da,wa,ck,il,nowystan

#ifdef A_LAN
DatY->(dbgoto(1))
#endif
      da:=max(DatE(),max(DatY->d_z_mies1,DatY->data_gran)+1)
      do while YEAR(da)>YEAR(DatY->D_Z_rok+1)
         da-=day(da)
      enddo

stat:=push_stat()

sel('firmy','firm_nip')

sel('stany','stan_mag')

sel('indx_mat','indx_num')
 
sel('main','main_nrk')

sel('dm','dm_fak')

dt:=if(ko="20",hb_stot(ko),DatE()-5)

hb_memowrit('of.json','filters='+hb_jsonencode({"status_id"=>14,"status_date"=>{'>=' => da}}))
pg:=0
pl:=1

DO WHILE pg<pl .and. curl('-G -d limit=50 -d @of.json -d page='+ltrim(str(++pg)),'orders')
     
  d:=hb_jsonDecode(memoread('orders.json'))
  pl:=hb_hgetdef(d,'pages',0)
  l1:=hb_hgetdef(d,'list',{})
  for each b in l1
    select dm
    set order to 5

    seek mag_biez+'E:'+(c:=b['order_id'])
    locate for smb_dow='PA' .and. val(pozycja)>0 while nr_mag=mag_biez .and. upper(nr_faktury)='E:'+c
    if found()
      da:=max(da,hb_ctot(b['status_date'],'YYYY-MM-DD'))
      LOOP
    endif
    hb_memowrit('pf.json','filters='+hb_jsonencode({'order_id' => val(c)}))

	og:=0
	ol:=1

	DO WHILE og<ol .and. curl('-G -d limit=50 -d @pf.json -d page='+ltrim(str(++og)),'order-products')
	   d:=hb_jsonDecode(memoread('order-products.json'))
	   ol:=hb_hgetdef(d,'pages',0)

	   l2:=hb_hgetdef(d,'list',{})
	   if empty(l2)
	      exit
	   endif
IF og = 1

    set order to 1
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
    field->transport:=hb_jsonEncode(b)
    field->data:=da
    field->data_vat:=da
    field->data_dost:=hb_ctod(b['date'],'YYYY-MM-DD')
    field->kto_pisal:=chr(255)+chr(0)
    field->nr_spec:='cena'

    d:=if('VAT'$b['notes'] .or. ascan(b['additional_fields'],{|x|x['value']=='1' .and. x['field_id']=='3'})>0,'F','')
    field->nr_faktury:='e:'+c+' '+d

    c:=STRTRAN(STRTRAN(b['billing_address','tax_identification_number'],' '),'-')
    if d == 'F'
       if !empty(c) .and. firmy->(ordsetfocus('firm_nip'),dbseek(c,.f.)) .and. trim(firmy->ident)==c
       elseif !empty(c) .and. firmy->(ordsetfocus('firm_naz'),dbseek(UpP(pad(b['billing_address','company'],len(nazwa)),.f.)))
	 LOCK IN FIRMY
	 firmy->ident:=c
         firmy->(ordsetfocus('firm_nip'))
	 UNLOCK IN FIRMY
       else
         firmy->(ordsetfocus('firm_num'))
         firmy->(dbgobottom())
         d:=val(firmy->numer_kol)+1 
         firmy->(dbappend())
	 firmy->numer_kol:=strtran(str(d,len(firmy->numer_kol)),' ','0')
         firmy->ident:=c
         firmy->nazwisko:=b['delivery_address','firstname']+' '+b['delivery_address','lastname']
	 if empty(firmy->longname:=b['billing_address','company'] )
	    firmy->longname:=b['billing_address','firstname']+' '+b['billing_address','lastname']
	 endif
	 firmy->nazwa:=firmy->longname
         firmy->adres:=b['billing_address','postcode']+' '+b['billing_address','city']+', '+b['billing_address','street1']+' '+b['billing_address','street2']
         firmy->(ordsetfocus('firm_nip'))
         UNLOCK IN FIRMY
       endif
       field->kontrahent:=firmy->numer_kol
       field->dost_odb:=b['billing_address','company']
       if empty(field->dost_odb:=b['billing_address','company'])
	  field->dost_odb:=b['billing_address','firstname']+' '+b['billing_address','lastname']
       endif
    else
      c:=''
      if b['user_id']<>NIL
        field->kontrahent:='@'+str(val(c:=b['user_id'])%10000,4)
      endif
      field->dost_odb:='e-sklep'+trim(' '+b['billing_address','firstname'])+trim(' '+b['billing_address','lastname'])+trim(' '+b['billing_address','company'])+' @'+c
    endif


    //field->nazwisko:=str(val(subs(c[35],21,6)),6)
 /*
 c:=mxmlgetcData(mxmlGetFirstChild( mxmlfindelement(b, b, "current_state",,,1 ) ))
 field->transport:=str(val(c),2)
 */

 field->nr_czeku:=b['payment_id']

 field->wartosc:=field->przelewem:=val(b['sum'])

 c:=val(b["shipping_cost"])
 select main
 
 IF c<>0
   indx_mat->(dbseek('000936 '))
   dbappend()
   field->nr_mag:=mag_biez
   field->smb_dow:='PA'
   field->nr_dowodu:=dm->nr_dowodu
   dm->pozycja:=field->pozycja:=str(1,len(pozycja))
   field->data:=dm->data
   field->cena:=c
   field->index:=indx_mat->index
   field->proc_vat:=indx_mat->proc_vat
   field->ilosc_f:=-1
   
   nowystan:= .not.stany->(dbseek(mag_biez+main->index,.f.))
   if nowystan .or. stany->(reclock())
#ifdef A_WA     
     il:=ilosc_f
     //da:=data
     wa:=ck:=0
     GETCK(.t.)
     e:=ROUND(IL*CK,2)
 #ifdef A_WE     
     DM->wart_ewid:=Round(DM->wart_ewid+e-wartosc,A_ZAOKR)
 #endif     
     STANY->WARTOSC:=round(STANY->wartosc+e-WARTOSC,A_ZAOKR)
     STANY->STANx:=Round(STANY->stanx+ILOSC_F-ILOSC,3)
     STANY->WARTOSCx:=round(STANY->wartoscx+e-WARTOSC,A_ZAOKR)
     MAIN->WARTOSC:=e
#endif     
     STANY->STAN:=Round(STANY->stan+ILOSC_F-ILOSC,3)
     MAIN->ILOSC:=ILOSC_F
     STANY->DATA_ROZ:=MAX(DA,STANY->DATA_ROZ)
     STANY->DATA_ZMIAN:=STANY->(MAX(DATA_ZMIAN,DATA_ROZ))
     STANY->unlink:=.t.
     STANY->(dbunlock())
   endif  
 ENDIF
endif

 FOR EACH a IN l2 

  if !indx_mat->(dbseek(a['code'],.f.))
    loop
  endif
  dbappend()
  field->nr_mag:=mag_biez
  field->smb_dow:='PA'
  field->nr_dowodu:=dm->nr_dowodu
  dm->pozycja:=field->pozycja:=str(val(dm->pozycja)+1,len(pozycja))
  field->data:=dm->data
  field->index:=indx_mat->index
  field->proc_vat:=indx_mat->proc_vat

  il:=-val(a['quantity'])

  c:=val(a['price'])*(100-val(a['discount_perc']))/100
  
  if indx_mat->(przel>1 .and. UpP(jm)<>jm_opcja .and. jm_opcja='SZT')
     il*=indx_mat->przel
     c/=indx_mat->przel
  endif
  
  field->ilosc_f:=il
  field->cena:=c
  
  nowystan:= .not.stany->(dbseek(mag_biez+main->index,.f.))
  if nowystan .or. stany->(reclock())
#ifdef A_WA
  //da:=data
  wa:=ck:=0
  GETCK(.t.)
  e:=ROUND(IL*CK,2)
 #ifdef A_WE 
     DM->wart_ewid:=Round(DM->wart_ewid+e-wartosc,A_ZAOKR)
 #endif     
     STANY->WARTOSC:=round(STANY->wartosc+e-WARTOSC,A_ZAOKR)
     STANY->STANx:=Round(STANY->stanx+ILOSC_F-ILOSC,3)
     STANY->WARTOSCx:=round(STANY->wartoscx+e-WARTOSC,A_ZAOKR)
     MAIN->WARTOSC:=e
#endif     
  STANY->STAN:=Round(STANY->stan+ILOSC_F-ILOSC,3)
  MAIN->ILOSC:=ILOSC_F
  STANY->DATA_ROZ:=MAX(DA,STANY->DATA_ROZ)
  STANY->DATA_ZMIAN:=STANY->(MAX(DATA_ZMIAN,DATA_ROZ))
  STANY->unlink:=.t.
  IF indx_mat->zapas_min>0 .and. indx_mat->zaznacz<>2 .and. STANY->stan<indx_mat->zapas_min .and. indx_mat->(dbrlock())
     indx_mat->zaznacz:=2
     indx_mat->(dbunlock())
  ENDIF
  
  STANY->(dbunlock())
  endif

 NEXT

ENDDO

 dt:=max(dt,hb_ctot(b['status_date'],'YYYY-MM-DD'))
 ko := hb_ttos(dt)
NEXT

ENDDO

UNLOCK IN DM
UNLOCK IN MAIN
UNLOCK IN STANY

pop_norec(stat)

return ko
************************************
func e_stock(all,h)
LOCAL xData, _id, name, i, stat,p
stat:= push_stat()


if valtype(all)<>'A'


  if empty(all)
    sel('stany','stan_chg')
  else 
    sel('indx_mat','indx_num')
    sel('stany','stan_mag')
    set relation to index into indx_mat
    set filter to indx_mat->zapas_id<>0  //.and. 'S'$indx_mat->rodz_tow
  endif


  seek mag_biez
 
  hb_default(@h,{})

  do while nr_mag=mag_biez

   if pcount()<2 .or. ascan(h,index)=0
     aadd(h,index) //{indx_mat->zapas_id,indx_mat->(if(jm_opcja='SZT.' .and. przel>1 .and. upper(jm)<>jm_opcja,przel,1)),index,indx_mat->id})
   endif
   skip

  end do

  if pcount()>1
    pop_stat(stat)   
    return h
  endif
  all:=h
endif

  sel('indx_mat','indx_num')
  sel('stany','stan_mag')
  set relation to index into indx_mat
  set order to tag stan_mag
  set filter to

  _id:=len(all) 

  for i:=_id to if(pcount()>1,max(1,_id-2),1) step -1
    select STANY
    seek mag_biez+all[i] //,3]
    select INDX_MAT 
    seek all[i]
    if zapas_id<>0
      p:=if(jm_opcja='SZT.' .and. przel>1 .and. upper(jm)<>jm_opcja,przel,1)
      xData:= {=>} 
      hb_hAutoAdd(xData,.t.)
      //xData['product_id'] := id
      //xData['price']  := if(proc_mar3>0,cena3,cena)*p
      //xData['weight'] := if(jm='kg',1,gram/1000)*p
      //xData['active']:= 1
      xData["availability_id"]:=IF('Z'$indx_mat->rodz_tow,8,)
      xData["stock"] := Round(max(0,stany->stan)/p,IF(przel<>1 .or. jm$'szt.,para,kpl',0,3))
      hb_memowrit('exp_sto.json',hb_jsonencode(xData))
      if !curl('-X PUT -d @exp_sto.json','product-stocks/'+ltrim(str(zapas_id)),,pcount()>1,.t.)
        exit
      endif
    endif
    select stany
    if unlink .and. dbrlock() 
       unlink := .f.
       dbunlock()
    endif
  next i

  asize(all,i)
  pop_stat(stat)   

return all
/***********************
func e_exp()

   LOCAL tree, node, group, element, i, j, k, ap,h
   LOCAL xData, _id, name, fota_id
   LOCAL stawki_zby:={'23',' 8',' 0','zw','np',' 5'}
   local hp:='.'+HB_OsPathSeparator()             //getenv('HOME')
   local pic:=defa+linpath('..\etykiety\kreski\')
   local stat




   if !file(defa+'taxes.json')
     curl('-G -d limit=50','taxes',defa+'taxes.json')
   endif
   h:=hb_jsondecode(memoread(defa+'taxes.json'))

   stawki_zby:={}
   aeval(h['list'],{|h|h:=h['name'],aadd(stawki_zby,if(h>'a',left(h,2),str(val(h),2)))})
   
   k:=0
   j:=1
   ap:={}   
   do while ++k<=j .and. (i:=defa+'products'+str(k/1000,5,3),.t.) //.and. curl('-G -d limit=50 -d page='+ltrim(str(k)),'products',i))
     h:=hb_jsondecode(memoread(i))
     j:=h['pages']
	 h:=h['list']
	 i:=len(ap)
	 asize(ap,i+len(h))
	 acopy(h,ap,,,i+1)
   end do
   
   if len(ap)=0
      return .f.
   endif 
   
   stat:=push_stat()   
   
   sel('stany','stan_ind')
   sel('indx_mat','indx_num')

   ? 'Kasowanie'
   ?
   ?

   for i:=len(ap) to 1 step -1
	    if !dbseek(ap[i,'code'],.f.) .or. ( !'S'$indx_mat->rodz_tow ) 
           ?? ap[i,'code'],ap[i,'translations','pl_PL','name']
           ?
           if curl('-X DELETE','products/'+ap[i,'product_id'],,,.t.)
              hb_adel(ap,i,.t.)
		   endif	  
        endif
     endif
   next

   set filter to !empty(id)
   go top

   do while !eof()
     //i:=ascan(ap,id)
	i:=ascan(ap,{|h|h['product_id']=id})
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
     if ascan(ap,{|h|h['product_id']=id})=0 .and. reclock()
     //if !indx_mat->(dbseek(e_prod->index,.f.) .and. id<>0) .and. reclock()
       DELETE
       dbunlock()
     endif
     skip
   enddo

   set order to tag id

   select indx_mat

   set filter to 'S' $ rodz_tow .and. empty(id)
   go top
   
****
? 'Nowe produkty:'
?
?
     do while !eof()

       e_prod->(dbgoto(0))

       ?? nazwa
       element := { => }
       do_aktual(element)
	   
       hb_memowrit('product.json',hb_jsonencode(element))
	   curl('-X POST -d @product.json','products','product_id.json',,.t.)
       LOCK 
	   node:=val(memoread('product_id.json'))

       if empty(node)
         i:=setcolor('R+')
         ?? ' - ERROR'
		 ?? memoread('product_id.json')
         setcolor(i)
         field->rodz_tow:=strtran(field->rodz_tow,'S',"X")
         field->zaznacz:=2
       else
         i:=setcolor('G+')
         ?? ' - OK'
         setcolor(i)
         field->id:=node
	     field->data_id:=date()
		 element['product_id']:=str(node)
		 element['stock'] :={'product_id' => node}
     	 do_aktuals(element['stock'])
         hb_memowrit('stock.json',hb_jsonencode(element['stock']))
	     curl('-X POST -d @stock.json','product-stocks','stock_id.json',,.t.)
   	     node:=val(memoread('stock_id.json'))
		 field->zapas_id:=node
       
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
		 aadd(ap,element)
       endif

       ?
   
       skip
     enddo

************************************
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
    _id := new_fota
    
    if '2'>_id  //data
       ?? nazwa 
       ? 'Kasowanie fotek:'
	   hb_memowrit('ff.json','filter='+hb_jsonencode({}))
	   curl('-G -d @ff.json','product-images','img_list.json')
	   
       h:=hb_jsondecode(memoread('img_list.json'))['list']
       
       for i:=1 to len(h)
	     ?? h[i,'gfx_id']+', ' 
         curl('-X DELETE','product-images/'+h[i,'gfx_id'])
       next   
       
    endif

    xData := directory((j:=pic+left(index,4)+HB_OsPathSeparator())+subs(trim(index),5)+"*.*")
   
    asort(xData,,,{|x,y|lower(x[1])>lower(y[1])})
    for i:=1 to len(xData)
       if dtos(xData[i,3])+' '+xData[i,4] > new_fota
         ?? nazwa
         ? 'fotka',xdata[i,1]
         node := { => }
		 hb_hautoadd(node,.t.) 
         node['product_id']:= id
         node['name']      := nazwa
         node['file']      := left(index,4)+lower(xdata[i,1])
         node['content']   := hb_base64encode(memoread(j+xData[i,1]))
         hb_memowrit('exp_pic.json',hb_jsonEncode(node))
         curl('-X POST -d @exp_pic.json','product-images','gfx_id.json')
       endif
	
	
         if xData[i,1]=trim(subs(index,5))+'.'
             fota_id := val(memoread('gfx_id.json'))	
             if fota_id<>0
               k:=setcolor('G+')
               ?? ' - OK'
               setcolor(k)
             else
               k:=setcolor('R+')
               ?? ' - ERROR'
               setcolor(k)
               fota_id:=NIL
             endif
         endif

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
   
*************
   element := { stock => { => }} //'product_id' => id , stock => {'stock_id' => zapas_id }}
   
*********************   
   do_aktual(element)
   do_aktuals(element['stock'])
**********************
   if !empty(fota_id)
     element['main_image'] := {'gfx_id' => fota_id}
   endif
**********************   
   hb_memowrit('product.json',hb_jsonencode(element))
   if curl('-X PUT -d @product.json','products/'+ltrim(str(id)),'product_id.json',,.t.)
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
   else	 
     k:=setcolor('R+')
     ?? ' - ERROR'
	 ?? memoread('product_id.json')
     setcolor(k)
     LOCK
     field->zaznacz:=2
   endif
   
   UNLOCK
   
   ?

   skip

enddo
*******/
//HTTP/1.1 401 Unauthorized
//Www-Authenticate: bearer realm="Service", error="unauthorized_client", error_description="Provided access token has expired"
//{"access_token":"48e0f66bc83e85c2906299c782c073d333dd89fe","expires_in":2592000,"token_type":"bearer"}
//curl -X POST -u admin:Shoper77 https://sklep963623.shoparena.pl/webapi/rest/auth

*************************
func getshop(line,res,ans)
local txt:='',h
curl(@line,res,@ans)
hb_jsondecode(memoread(ans),@h)
txt:=hb_jsonencode(h,.t.)
return txt
**************************
func curl(line,res,ans,no_wait,no_error)
static token
local ok:=.f.,head,a,b,c,h:=-1,e

if token=NIL .and. !empty(a:=memoread(defa+'token.json'))
   token:=hb_hGetDef(hb_jsondecode(a),'access_token',NIL)
endif

hb_default(@line,'')

if empty(ans)
  a:=at('/',res)
  ans:=if(a=0,res,left(res,a-1))+'.json'
endif

e:=errorblock({|e|break(e)})
begin sequence
while .t.
  if h>0
    fclose(h)
    h:=0
    ferase(defa+'curl.lock')
  endif    

  if file(defa+'curl.lock') .and. (hb_fGetDateTime( defa+'curl.lock', @a, @b),b:=int(secs(b))+val(subs(b,9)),c:=max(0,int(seconds()+86400-b)%86400*2),c<10) //secs obcina setne  
     if !empty(no_wait) .or. nextkey()=27
        exit
     endif
     hb_idlesleep(.3)
     loop 
  endif

  if waitshop(no_wait,head)=0 .or. nextkey()=27
     exit
  endif

  h:=fcreate(defa+'curl.lock') 

  if h=-1
     if !empty(no_wait) .or. nextkey()=27
        exit
     endif
     hb_idlesleep(.3)
//     ferase(defa+'curl.lock')
     loop 
  endif  
  ferase(ans)
  if token=NIL
     head:='HTTP/1.1 401 access token '
  else
//  ferase(defa+[header.txt])
    hb_run('curl --insecure -D '+defa+'header.txt -H "Authorization: Bearer '+token+'" -u dim:Shoper77 '+line+' https://zagroda.cieszyn.pl/webapi/rest/'+res+' > '+ans)
    head:=memoread(defa+[header.txt])
  endif
  c:=val(subs(head,at(' ',head)+1,5))
  if !empty(no_error) .or. c>=100 .and. c<=200 
     ok:=.t.
     exit 
  elseif c=401 .and. " access token " $ head
     c:=if(empty(c:=memoread(defa+'token.json')),token,hb_hGetDef(hb_jsondecode(c),'access_token',token))
     if c<>token
        token:=c
        loop
     endif
     ferase(defa+'token.json')
     ferase(defa+'header.txt')
     hb_run([curl --insecure -D ]+defa+[header.txt -u dim:Shoper77 -X POST https://zagroda.cieszyn.pl/webapi/rest/auth > ]+defa+[token.json])
     c:=if(empty(c:=memoread(defa+'token.json')),NIL,hb_hGetDef(hb_jsondecode(c),'access_token',NIL))
     if !empty(c)
      token:=c
      loop
     endif
  endif 
  if !empty(head)
        head:=getlines(head)[1] 
  endif
  IF hb_jsondecode(memoread(ans),@c)>0
        head+=hb_jsonencode(c,.t.)
  ENDIF
  alarm(head)
  exit
end

  recover using c
    errorblock(e)
    if h>0
      fclose(h)
      h:=0
    endif 
    ferase(defa+'curl.lock')
    eval(e,c)
  end sequence 
  if h>0
    fclose(h)
  endif 
  ferase(defa+'curl.lock')
  errorblock(e)

return ok

func waitshop(no_wait,e)
local n,d,f,g
        while (hb_fGetDateTime( defa+'header.txt', @d, @g),g:=int(secs(g))+val(subs(g,9)),n:=max(0,int(seconds()+86400-g)%86400*2),n<10) //secs obcina setne
          if empty(d) .and. empty(e)
             n:=10
             exit
          endif
          if f<>NIL .and. f<>g .or. empty(e)
             e:=NIL
          endif 
          hb_default(@e,memoread(defa+'header.txt'))
          d:=val(subs(e,at('Limit:',e)+6,3))
          if empty(d)
            n:=0
            e:=NIL
          else 
            f:=g
            d-=val(subs(e,at('Calls:',e)+6,3))
            n+=d
            if n>1
              exit
            endif
          endif
          if !empty(no_wait) .or. nextkey()=27
             exit
          endif
          hb_idlesleep(.3)
        end
return min(10,n)

