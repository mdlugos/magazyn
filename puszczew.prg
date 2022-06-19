#require "hbmxml"

field data,nr_mag,index,ilosc,ilosc_f,wartosc,stan,stanx,wartoscx,pozycja,nr_dowodu,smb_dok,id,;
	proc_vat,data_zmian,data_roz,unlink,nazwa,cena,cena3,proc_mar3,parent,rodz_tow,new_fota,grupa,jm,gram,;
	uwagi,grupy,zapas_id,nr_faktury,data_id,przel,jm_opcja
memvar dokumenty,mag_poz,mag_biez,dzisiaj,defa

func e_edi(ko)
local a,b,c,d,e,f
MEMVAR r,da,wa,ck,il,nowystan,avat

DO WHILE .T.

 IF !file(ko)
   EXIT
 ENDIF
 a:=mxmlNewXML()
 a:=b:=mxmlLoadFile(a,ko)
 if empty(b)
   EXIT
 endif
 
 b:=mxmlfindelement(b, a, "invoiceIdentification",,,1 )
 
 if empty(b)
   EXIT
 endif
 

 dbselectar(select('i_odczyt'))
 use (defa+'odczyt'+HB_ps()+'i_odczyt') 

 append blank
 
 field->data:=DatE()

 b:=mxmlfindelement(b, a, "uniqueCreatorIdentification",,,1 )
 //c:=mxmlGetFirstChild(b)
 c:=mdgetTxt( b )
 field->nr_faktury:=c

 b:=mxmlfindelement(b, a, "SalesDateYMD",,,1 )
 //c:=mxmlGetFirstChild(b)
 c:=mxmlgetText( b )
 
 field->data_dost:=stod(strtran(c,'-'))

 b:=mxmlfindelement(b, a, "seller",,,1 )

 b:=mxmlfindelement(b, a, "NIP",,,1 )
 //c:=mxmlGetFirstChild(b)
 c:=mxmlgetText( b )
 sel('firmy','firm_nip')
 if dbseek( c ,.f.)
   i_odczyt->dost_odb:=firmy->nazwa
   i_odczyt->kontrahent:=firmy->numer_kol
 else
   i_odczyt->dost_odb:=c
 endif

 select i_odczyt
 dbcreate(c:=defa+'odczyt'+HB_ps()+'i'+strtran(str(lastrec(),7),' ','0'),{{'index','C',10,0},{'nazwa','C',46,0},{'cena','N',10,2},{'proc_vat','C',2,0},{'ilosc','N',10,3},{'jm','C',4,0}})

 dbselectar(select('i_od0'))
 use (c) alias i_od0
 c:=b:=a
 DO WHILE !empty( c:=mxmlfindelement(c, b, "invoiceLineItem",,,1 ))

  d:=mxmlfindelement(c, c, "itemName",,,1 )
  IF EMPTY(d)
    EXIT
  ENDIF
  dbappend()
  e:=mdgetTxt( d )
  field->index:=e

  d:=mxmlfindelement(c, c, "text",,,1 )

  e:=mdgetTxt( d )
  field->nazwa:=e
    
  d:=mxmlfindelement(c, c, "value",,,1 )
  e:=val(mxmlgetText( d ))
  field->ilosc:=e

  d:=mxmlfindelement(c, c, "measurementUnitCodeValue",,,1 )
  e:=mxmlgetText( d )
  field->jm:=Lower(e)

  d:=mxmlfindelement(c, c, "taxRate",,,1 )
  e:=val(mxmlgetText( d ))
  field->proc_vat:=str(e,2)
    
  d:=mxmlfindelement(c, c, "invoiceLineNettValue",,,1 )
  e:=val(mxmlgetText( d ) )
  field->cena:=e

  d:=mxmlfindelement(c, c, "itemSalesDate",,,1 )
  e:=mxmlgetText( d )
  i_odczyt->data:=stod(strtran(e,'-'))

 ENDDO

 use
 select i_odczyt

  b:=mxmlfindelement(a, a, "timePeriodDue",,,1)
  b:=mxmlfindelement(b, a, "value",,,1 )
  //c:=mxmlGetFirstChild(b)
  c:=val(mxmlgetText( b ))
  i_odczyt->termin_p:=i_odczyt->data_dost+c
 
 use

 exit 

ENDDO

mxmldelete(a)

return .t.
*******************
stat func mdgettxt(n)
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
/************************
func from_xml(x)
//    x:=strtran(x,'&lt;','<')
//    x:=strtran(x,'&gt;','>')
//    x:=strtran(x,'&apos;',"'")
//    x:=strtran(x,'&quot;','"')
//    x:=strtran(x,'&amp;','&')
return hb_translate(x,'UTF8',)
*/
