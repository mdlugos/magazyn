#include "error.ch"

#require "hbmxml"

#ifdef __PLATFORM__UNIX_
#require "hbssl"
#require "hbcurl"
#endif

#define D_KSEF_VARIANT 2
#define D_KSEF_API MEMVAR->ksef_api
//#define D_KSEF_API 'https://ksef.mf.gov.pl/'

#define D_MAGVARIANT "1"
#define D_MAGNAMESPACE "http://jpk.mf.gov.pl/wzor/2016/03/09/03093/"

//#define D_FAVARIANT 1
//#define D_FANAMESPACE "http://jpk.mf.gov.pl/wzor/2016/03/09/03095/"
//#define D_FASCHEMA "http://crd.gov.pl/xml/schematy/dziedzinowe/mf/2016/01/25/eD/DefinicjeTypy/"

#define D_FAVARIANT 4
#define D_FANAMESPACE "http://jpk.mf.gov.pl/wzor/2022/02/17/02171/"
#define D_FASCHEMA "http://crd.gov.pl/xml/schematy/dziedzinowe/mf/2018/08/24/eD/DefinicjeTypy/"

#define D_ZAKOFFSETold 42
#define D_ZAKOFFSETnew 39
#define D_ZAKOFFSET if(dekl,D_ZAKOFFSETnew,D_ZAKOFFSETold)
#define D_SPRZEDOFFSET 9

#define D_WARIANT 3

#define DD_WARIANT ver

#define DD_SCHEMA    ({"1-2E","1-0E"}[ver])
#define DD_NAMESPACE ({"http://crd.gov.pl/wzor/2020/05/08/9393/","http://crd.gov.pl/wzor/2021/12/27/11148/"}[ver])

#if D_WARIANT == 2
#define D_SCHEMA "1-0"
#define D_NAMESPACE "http://jpk.mf.gov.pl/wzor/2016/10/26/10261/"
#else
#define D_SCHEMA "1-1"
#define D_NAMESPACE "http://jpk.mf.gov.pl/wzor/2017/11/13/1113/"
#endif

external hb_jsondecode,hb_jsonencode,hb_hAutoAdd,hb_hclone,hb_hhaskey
external hb_base64encode,hb_sha256,URLENCODE

static tree,jpk,lp,aj //aj w groszach
static group,dekl := .f.,groupd,ver
static token := {=>}
memvar mag_poz,mag_biez,magazyny,adres_mag,defa,_snorm

#ifdef A_KSEF
**************************
func curl(res,line,post,ans)

#ifdef __PLATFORM__UNIX_
   local curl,a:=getlines(line,' -H ')
   curl_global_init()
   curl := curl_easy_init()
   curl_easy_setopt( curl, HB_CURLOPT_URL, D_KSEF_API + 'api/online/' + res )
   curl_easy_setopt( curl, HB_CURLOPT_DL_BUFF_SETUP )
   curl_easy_setopt( curl, HB_CURLOPT_HEADER )
   if post<>NIL
      curl_easy_setopt( curl, HB_CURLOPT_UL_BUFF_SETUP, post )
      curl_easy_setopt( curl, if(a[1]='-X POST', HB_CURLOPT_POST, HB_CURLOPT_PUT ))
   endif
   hb_adel(a,1,.t.)
   curl_easy_setopt( curl, HB_CURLOPT_HTTPHEADER, a )
   if curl_easy_perform( curl )=0
      ans:=curl_easy_dl_buff_get( curl )
   endif
   curl_easy_cleanup( curl )
   curl_global_cleanup()
#else
    DEFAULT line TO ''
    if !'-D'$ line
      line+=' -i'
    endif
    if !empty(post)
      line+=' --data-binary @-'
    endif
    hb_processrun('curl -s '+line+' '+ D_KSEF_API + 'api/online/' + res,post,@ans)
#endif
return ans

static func token(bin)
if type('memvar->ksef_token')='C'
   return memvar->ksef_token
endif
return hb_BSubStr(bin,HB_HEXTONUM(hb_BSubStr(bin,1,2))+1,HB_HEXTONUM(hb_BSubStr(bin,3,2)))

func ksef_initsession()
local ans,s,i,j,nip
//    local scr

if empty(token)
   hb_hautoadd(token,.t.)
   if type('memvar->ksef_token')='C'
      token['token']:=memvar->ksef_token
   else
      s:=select()
      sel('MAGAZYNY')
      if type('_FIELD->ksef_token')='U'
         USE
         dbselectarea(s)
         return NIL
      endif
      LOCATE FOR _FIELD->numer=mag_biez
      token['token']:=token(_FIELD->ksef_token)
      if s<>select()
         USE
         dbselectarea(s)
      endif
   endif
   s:=findfile('session.json')
   s:=memoread(s)
   if s=hb_utf8Chr(0xFEFF)
      s:=hb_bsubstr(s,4)
   endif
   token['sessiontoken']:=if(empty(s),,hb_jsondecode(s))
endif

//    save screen to scr
//    set color to (_snorm)
//    clear screen

if !empty(token['sessiontoken'])
   curl('Session/Status/'+token['sessiontoken','referenceNumber'],'-X GET -H sessionToken:'+token['sessiontoken','sessionToken','token'],,@ans)
   i:=at(' 200 ',memoline(ans,,1))
   if i=0
     token['sessiontoken']:=NIL
     s:=findfile('session.json')
     if !empty(s)
        ferase(s)
     endif
   endif
endif
if empty(token['sessiontoken'])
   nip:=trim(strtran(memvar->firma_NIP,'-'))
   curl('Session/AuthorisationChallenge','-X POST -H Content-Type:application/json',hb_jsonencode({"contextIdentifier"=>{"type"=>"onip","identifier"=>nip}}),@ans)
   j:=at('{',ans)
   if j<=5 .or. empty(j:=hb_jsondecode(substr(ans,j))) .or. !hb_hhaskey(j,"timestamp")
     alarm(ans)
     //restore screen from scr
     return NIL
   endif
   token['Challenge']:=j
   s:=token['token']+"|"+hb_ntoc(hb_ttomsec(hb_ctot(j["timestamp"],"yyyy-mm-dd","HH:MM:SS.FFF"))-hb_ttomsec(0d19700101),0)
#ifdef __PLATFORM__UNIX_
   SSL_init()
   OpenSSL_add_all_ciphers()
   ctx := hb_EVP_CIPHER_ctx_create()
   EVP_CIPHER_CTX_init( ctx )
   EVP_EncryptInit( ctx, "", hb_memoread(findfile(MEMVAR->ksef_key)) )
   EVP_CIPHER_CTX_SET_PADDING(ctx,HB_RSA_PKCS1_PADDING)
#else   
   s:=hb_processrun('openssl pkeyutl -encrypt -pubin -pkeyopt rsa_padding_mode:pkcs1 -inkey '+findfile(MEMVAR->ksef_key),s,@ans)
#endif   
   if s<>0
      alarm('openssl '+hb_ntoc(s,0))
      //restore screen from scr
      return NIL
   endif
   ans:=HB_BASE64ENCODE(ans)
   s:=findfile("InitSessionTokenRequest.xml")
   s:=MEMOREAD(s)
   if s=hb_utf8Chr(0xFEFF)
      s:=hb_bsubstr(s,4)
   endif
   i:=at('<Challenge></Challenge>',s)+11
   if i>20
        s:=stuff(s,i,0,j["challenge"])
   endif
   i:=at(':Identifier></',s)+12
   if i>20
      s:=stuff(s,i,0,nip)
   endif
   i:=at('<Token></Token>',s)+7
   if i>20
      s:=stuff(s,i,0,ans)
   endif
   curl('Session/InitToken','-X POST -H Content-Type:application/octet-stream',s,@ans)
   s:=memoline(ans,,1)
   if ' 201 '$s .or. ' 100 '$s
   else
      alarm(ans)
      //restore screen from scr
      return NIL
   endif
   j:=at('{',ans)
   ans:=SubStr(ans,j)
   hb_memowrit(defa+'session.json',ans)
   token['sessiontoken']:=hb_jsondecode(ans)
   HB_IDLESLEEP(1) // jak za szybko to błąd
endif
//restore screen from scr
return token['sessiontoken','sessionToken','token']

//{'method': 'get', 'url': '/online/Invoice/Get/5480008032-20240304-41DFD1115B22-3F', 'headers': {'Accept': 'application/octet-stream'}}
func ksef_getfa(b,d,ans)
local scr,i
   //save screen to scr
   //set color to (_snorm)
   //clear screen
   b:=trim(b)
   if len(b)<35
     return NIL
   endif
   DEFAULT d TO ksef_initsession()
   IF d=NIL
      //restore screen from scr
      RETURN NIL
   endif

   curl('Invoice/Get/'+b,'-X GET -H sessionToken:'+d,,@ans)
   //restore screen from scr
   //i:=at('<?xml',ans)
   i:=at('<',ans)
   if empty(i)
      //hb_memowrit('get.txt',ans,.f.)
      alarm(ans)
      return NIL
   endif

   ans:=SubStr(ans,i)
return ans

func ksef_sendfa(c,b,d)
local a,ans,i,scr

   if len(c)<1024
      c:=hb_memoread(c) //no BOM removal this time
   endif

   b:={'invoiceHash'=>{'fileSize'=>hb_blen(c), 'hashSHA'=> {'algorithm'=> 'SHA-256', 'encoding'=> 'Base64', 'value'=> HB_BASE64ENCODE(HB_SHA256(c,.t.))}}, 'invoicePayload'=> {'type'=> 'plain', 'invoiceBody'=>HB_BASE64ENCODE(c)}}
   hb_hautoadd(b,.t.)

   //save screen to scr
   //set color to (_snorm)
   //clear screen
   DEFAULT d TO ksef_initsession()
   IF d=NIL
      //restore screen from scr
      RETURN NIL
   endif

   curl('Invoice/Send','-X PUT -H Content-Type:application/json -H sessionToken:'+d,hb_jsonencode(b),@ans)
   if empty(i:=at('{',ans)) .or.;
      empty(i:=hb_jsondecode(SubStr(ans,i))) .or.;
      empty(i:=hb_HGetDef(i,'elementReferenceNumber',''))
      hb_memowrit('send.txt',ans)
      alarm(ans)
      //restore screen from scr
      return NIL
   endif
   a:='Invoice/Status/'+i
   do while .t.
      HB_IDLESLEEP(1)
      curl(a,'-X GET -H sessionToken:'+d,,@ans)
      i:=at('{',ans)
      i:=hb_jsondecode(SubStr(ans,i))
      //"invoiceStatus":{"invoiceNumber":"FA6","ksefReferenceNumber"
      c:=hb_HGetDef(i,"invoiceStatus",{=>})
      if !empty(c) .and. hb_HHasKey(c,"ksefReferenceNumber")
         b["invoiceStatus"]:=c
         //restore screen from scr
         return c["ksefReferenceNumber"]
      endif
      c:=hb_HGetDef(i,"processingCode",400)
      if c>=400
        hb_memowrit('status.txt',ans)
        alarm(ans)
        //restore screen from scr
        return NIL
      endif
   enddo

//restore screen from scr
return NIL


//http://www.e-deklaracje.mf.gov.pl/Repozytorium/Slowniki/KodyKrajow_v3-0.xsd
static func nip2kraj(nip)
     nip:=Upper(left(alltrim(nip),2))
     if IsDigit(nip) .or. len(nip)<2 
        return "PL"
     endif
return nip

static func uwagi2odb(uwagi)
     local a:={},s,i:=0,l
     uwagi:=alltrim(uwagi)
     if lower(uwagi)='odbiorca' 
          uwagi:=ltrim(hb_BSubStr(uwagi,10))
          uwagi:=strtran(uwagi,';',HB_EOL())
          l:=mlcount(uwagi,254,,.f.)     
          for i:=1 to l
               s:=trim(memoline(uwagi,254,i,,.f.))
               if ''<>s
                    aadd(a,s)
               endif
          next
          l:=len(a)
          if l<2
               alarm(hb_UTF8ToStr('Odbiorca nie daje się podzielić na nazwę i adres, wstaw średnik:;')+uwagi)
          elseif l<3
               s:=a[1]
               a:=adres2arr(a[2])
               hb_ains(a,1,s,.t.)
          endif
     endif
return a

static func adres2arr(adres)
     local a,i,l,s
     adres:=alltrim(adres)
     a:=getlines(adres,',')
     i:=len(a)
     l:=lower(adres)
     if i<2 .and. 0<>ascan({' ul.',' pl.',' plac ',' al.',' rynek ',' alej',' '},{|x|i:=hb_at(x,l,9),i>0})
          a:={substr(adres,i+1),left(adres,i-1)}
     elseif i=2
          s:=a[1]
          a[1]:=alltrim(a[2])
          a[2]:=s
     else
          s:=a[1]
          a[1]:=alltrim(a[2])
          aeval(a,{|x|a[1]+=', '+alltrim(x)},3)
          asize(a,2)
          a[2]:=s
     endif
     aeval(a,{|x,i|a[i]:=alltrim(x)})
     if len(a)<>2
          alarm(hb_UTF8ToStr('Adres nie daje się podzielić na dwie części, wstaw przecinek:;')+adres)
     endif
return a

func ksef_fah()
     local a:=adres2arr(firmy->adres),b:=uwagi2odb(firmy->uwagi)
     if len(b)>0
        b:={hb_hash("IDNabywcy",,"NrEORI",,;
          "DaneIdentyfikacyjne",hb_hash("NIP",,"IDWew",,"KodUE",,"NrVatUE",,;
             "KodKraju",,"NrID",,"BrakID",1,"Nazwa",b[1]),;
          "Adres",hb_hash("KodKraju",nip2kraj(firmy->ident),"AdresL1",b[2],"AdresL2",b[3],"GLN",),;
          "AdresKoresp",,"DaneKontaktowe",,"Rola",2,"RolaInna",,"OpisRoli",,"Udzial",,"NrKlienta",)}
     endif

return hb_hash('Naglowek',hb_hash("KodFormularza","FA","WariantFormularza",D_KSEF_VARIANT,;
          "DataWytworzeniaFa",stuff(HB_TSTOSTR(HB_TSTOUTC(hb_DateTime())),11,1,'T')+'Z',;
          "SystemInfo",A_STOPKA),;
          'Podmiot1',hb_hash("PrefiksPodatnika",,"NrEORI",,;
               "DaneIdentyfikacyjne",{"NIP"=>trim(strtran(memvar->firma_NIP,'-')),"Nazwa"=>Trim(memvar->firma_pelnaz)},;
               "Adres",hb_hash("KodKraju","PL","AdresL1",trim(memvar->firma_Ul)+" "+trim(memvar->firma_Dom),"AdresL2",trim(memvar->firma_poczta),"GLN",),;
               "AdresKoresp",,"DaneKontaktowe",{hb_hash('Email',memvar->firma_email)},"StatusInfoPodatnika",),;
          'Podmiot2',hb_hash("NrEORI",,"DaneIdentyfikacyjne",{"NIP"=>Trim(strtran(firmy->ident,'-','')),"Nazwa"=>Trim(firmy->longname)},;
               "Adres",hb_hash("KodKraju",nip2kraj(firmy->ident),"AdresL1",a[1],"AdresL2",a[2],"GLN",),;
               "AdresKoresp",,"DaneKontaktowe",{},"NrKlienta",,"IDNabywcy",),;
          'Podmiot3',b,'PodmiotUpowazniony',,'Fa',;
          hb_hash("KodWaluty","PLN","P_1",,"P_1M",,"P_2",,"WZ",{},;
          "P_6",,"OkresFa",,"P_13_1",,"P_14_1",,"P_14_1W",,"P_13_2",,"P_14_2",,"P_14_2W",,;
          "P_13_3",,"P_14_3",,"P_14_3W",,"P_13_4",, "P_14_4",,"P_14_4W",,;
          "P_13_5",,"P_14_5",,"P_13_6_1",,"P_13_6_2",,"P_13_6_3",,;
          "P_13_7",,"P_13_8",,"P_13_9",,"P_13_10",,;
          "P_13_11",,"P_15",,"KursWalutyZ",,"Adnotacje",;
               hb_hash("P_16",2,"P_17",2,"P_18",2,"P_18A",2,"Zwolnienie",{"P_19N"=>1},"NoweSrodkiTransportu",{"P_22N"=>1},"P_23",2,"PMarzy",{"P_PMarzyN"=>1}),;
          "RodzajFaktury","VAT","PrzyczynaKorekty",,"TypKorekty",,;
          "DaneFaKorygowanej",,"OkresFaKorygowanej",,"NrFaKorygowany",,;
          "Podmiot1K",,"Podmiot2K",,"P_15ZK",,"KursWalutyZK",,"ZaliczkaCzesciowa",{},"FP",,;
          "TP",,"DodatkowyOpis",{},"FakturaZaliczkowa",{},"ZwrotAkcyzy",,;
          "FaWiersz",{},"Rozliczenie",,"Platnosc",,"WarunkiTransakcji",,"Zamowienie",),;
     "Stopka",)

func ksef_fa(fa, filen)
local element, node, s

     DEFAULT filen TO str(year(_FIELD->Data)%100,2)+strtran(trim(_FIELD->smb_dow+_FIELD->nr_dowodu),' ','0')+".xml"

     if tree<>NIL
          alarm(hb_UTF8ToStr("Poprzedna faktura nie zakończona"))
          break
          return nil
     endif

     tree:= mxmlNewXML()

     jpk := mxmlNewElement( tree, 'Faktura' )

     mxmlElementSetAttr( jpk, "xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")
     mxmlElementSetAttr( jpk, HB_EOL()+chr(9)+"xmlns:xsd", "http://www.w3.org/2001/XMLSchema")
     mxmlElementSetAttr( jpk, HB_EOL()+chr(9)+"xmlns", "http://crd.gov.pl/wzor/2023/06/29/12648/")

     element   := mxmlNewElement( jpk, 'Naglowek' )

     node := mxmlNewElement( element, "KodFormularza")
      mxmlElementSetAttr( node, "kodSystemowy", "FA ("+str(fa['Naglowek','WariantFormularza'],1)+")")
      mxmlElementSetAttr( node, "wersjaSchemy", "1-0E")
      mxmlNewText( node,, fa['Naglowek','KodFormularza'])

     node := mxmlNewElement( element, "WariantFormularza")
      mxmlNewText( node,, str(fa['Naglowek','WariantFormularza'],1))

     node := mxmlNewElement( element, "DataWytworzeniaFa")
      mxmlNewText( node,, fa['Naglowek',"DataWytworzeniaFa"])

     if fa['Naglowek',"SystemInfo"]<>NIL
      node := mxmlNewElement( element, "SystemInfo")
      mxmlNewText( node,, fa['Naglowek',"SystemInfo"])
     endif 

     addtree(jpk,fa,hb_HPos(fa,'Podmiot1'))

     mxmlsetwrapmargin(250)
     if len(filen) < 1024
        s:=memvar->defa+filen
        mxmlSaveFile( tree, s , @wscb() )
     else
        mxmlSaveString( tree, @filen , @wscb() )
        s:=filen
     endif
     
     mxmlDelete(tree)
     tree:=NIL
     //SET DATE A_SET_DAT

return s
#endif //KSEF
//json2xml
static function addtree(node,subtree,i)
local j,e,t:=valtype(subtree),k,v
DEFAULT i TO 1
  switch t
    case 'H'
        while i<=len(subtree)
            v:=hb_HValueAt(subtree,i)
            if empty(v) .and. valtype(v)<>'N'
               hb_hDelAt(subtree,i)
               loop
            endif
            k:=trim(hb_HKeyAt(subtree,i))
            if valtype(v)='A'
               j:=1
               while j<=len(v)
                  if v[j]=NIL
                     hb_adel(v,j,.t.)
                     loop
                  endif
                  addtree(mxmlNewElement( node, k),v[j])
                  ++j
               enddo
            else
               addtree(mxmlNewElement( node, k),v)
            endif
            ++i
        enddo
        exit
    case 'A'
         while i<=len(subtree)
            if subtree[i]=NIL
               hb_adel(subtree,i,.t.)
               loop
            endif
            addtree(node ,subtree[i])
            ++i
         enddo
       exit
    otherwise
       mxmlNewText(node,,ltrim(tran(subtree,)))
  end switch

return node


static func getnode(node,f)
local c,d,a,b,n,h
   if empty(node)
      return h
   endif
   n := mxmlGetElement(node)

   DEFAULT F TO ''

   if !f==n
      h:={=>}
      hb_hautoadd(h,.t.)
   endif

   do while !empty(n)
      a:={}
      do while !empty(node)
         c:=mxmlGetElement(node)
         if c==n
            d:=mxmlGetOpaque( node )
            if empty(d)
               b :=mxmlGetFirstChild( node )
               if !empty(b)
                  b :=mxmlGetNextSibling( b )
               endif
               d :=getnode( b )
            else
               if !empty(b:=hb_ctot(d,"YYYY-MM-DDThh:mm:ss.fffZ")) .and. hb_TtoN(b)%1<>0
                  d:=b
               elseif !empty(b:=hb_ctod(d,"YYYY-MM-DD"))
                  d:=b
               elseif tran(b:=val(d),)==d .and. ('.'$d .or. len(d)<9)
                  d:=b
               endif
            endif
            if !empty(d)
               aadd(a,d)
            endif
         elseif !empty(c)
            exit
         endif
         node:=mxmlGetNextSibling( node )
         c:=''
      enddo
      if len(a)>0
         if len(a)<2
            a:=a[1]
         endif
      endif
      if c<>f .and. f==n
         return a
      endif
      h[n]:=a
      n:=c
   enddo

return h

func xml2json(xml,e)
local a,t:=mxmlNewXML()
   a:=mxmlLoadString(t,xml,MXML_OPAQUE_CALLBACK)
   if empty(a)
      mxmlDelete(t)
      return NIL
   endif
   if empty(e)
      e:=''
      if !empty(a :=mxmlGetFirstChild( a ))
         if !empty(a :=mxmlGetNextSibling( a ))
            a :=mxmlGetNextSibling( a )
         endif
      endif
   else
      a:=mxmlfindelement(a,a,e,,,MXML_DESCEND)
   endif
   if !empty(a)
      a:=getnode(a,e)
   endif
   mxmlDelete(t)
return a
   
func jpk_krnagl(od,do,waluta)
local element,node

    if tree<>NIL
      mxmlDelete(tree)
      tree:=NIL
    endif

    tree:= mxmlNewXML()
    //set date format to "YYYY-MM-DD"
    jpk := mxmlNewElement( tree, 'JPK' )
        mxmlElementSetAttr( jpk,'xmlns:etd',"http://crd.gov.pl/xml/schematy/dziedzinowe/mf/2016/01/25/eD/DefinicjeTypy/")
        mxmlElementSetAttr( jpk,HB_EOL()+chr(9)+'xmlns',"http://jpk.mf.gov.pl/wzor/2016/03/09/03091/")


    element   := mxmlNewElement( jpk, 'Naglowek')

     node := mxmlNewElement( element, "KodFormularza")
      mxmlElementSetAttr( node, "kodSystemowy", "JPK_KR (1)" )
      mxmlElementSetAttr( node, "wersjaSchemy", "1-0" )
      mxmlNewText( node,, "JPK_KR")

     node := mxmlNewElement( element, "WariantFormularza")
      mxmlNewText( node,, "1")

     node := mxmlNewElement( element, "CelZlozenia")
      mxmlNewText( node,, "1")

     node := mxmlNewElement( element, "DataWytworzeniaJPK")
      mxmlNewText( node,, stuff(HB_TSTOSTR(HB_TSTOUTC(hb_DateTime())),11,1,'T')+'Z')

     node := mxmlNewElement( element, "DataOd")
      mxmlNewText( node,, hb_dtoc(od,'YYYY-MM-DD') )

     node := mxmlNewElement( element, "DataDo")
      mxmlNewText( node,, hb_dtoc(do,'YYYY-MM-DD') )

DEFAULT waluta TO 'PLN'

     node := mxmlNewElement( element, "DomyslnyKodWaluty")
      mxmlNewText( node,, waluta )

     node := mxmlNewElement( element, "KodUrzedu")
      mxmlNewText( node,, Trim(memvar->firma_Urz) )

    group := mxmlNewElement( jpk, 'Podmiot1' )

     element := mxmlNewElement( group, 'IdentyfikatorPodmiotu' )

      node := mxmlNewElement( element, "etd:NIP")
       mxmlNewText( node,, Trim(memvar->firma_NIP) )

      node := mxmlNewElement( element, "etd:PelnaNazwa")
       mxmlNewText( node,, Trim(memvar->firma_PelNaz) )

      if !empty(memvar->firma_REGON)
      node := mxmlNewElement( element, "etd:REGON")
       mxmlNewText( node,, trim(memvar->firma_REGON) )
      endif
     element := mxmlNewElement( group, 'AdresPodmiotu' )

      node := mxmlNewElement( element, "etd:KodKraju")
       mxmlNewText( node,, "PL" )

      node := mxmlNewElement( element, "etd:Wojewodztwo")
       mxmlNewText( node,, trim(memvar->firma_Woj) )

      node := mxmlNewElement( element, "etd:Powiat")
       mxmlNewText( node,, trim(memvar->firma_Pow) )

      node := mxmlNewElement( element, "etd:Gmina")
       mxmlNewText( node,, Trim(memvar->firma_Gmin) )

      if !empty(memvar->firma_ul)
      node := mxmlNewElement( element, "etd:Ulica")
       mxmlNewText( node,, Trim(memvar->firma_Ul) )
      endif

      node := mxmlNewElement( element, "etd:NrDomu")
       mxmlNewText( node,, TRim(memvar->firma_Dom) )

      node := mxmlNewElement( element, "etd:Miejscowosc")
       mxmlNewText( node,, Trim(memvar->firma_miasto) )

      node := mxmlNewElement( element, "etd:KodPocztowy")
       mxmlNewText( node,, left(memvar->firma_poczta,6) )

      node := mxmlNewElement( element, "etd:Poczta")
       mxmlNewText( node,, trim(SubStr(memvar->firma_poczta,8)) )

      aj:=lp:=NIL


return jpk


func jpk_krZOiS(kodkonta,opiskonta,typkonta,kodzespolu,opiszespolu,kodkategorii,opiskategorii,bown,boma,obwn,obma,obwnn,obman,salwn,salma)
local node

      if pcount()=0
      /*
          group := mxmlNewElement( jpk, "DziennikCtrl")
               node   := mxmlNewElement( group, 'LiczbaWierszyDziennika' )
          mxmlNewText( node,,lTrim(sTr(lp)))
               node   := mxmlNewElement( group, 'SumaKwotOperacji' )
          mxmlNewText( node,,ltrim(tran(aj/100,)))
      */
         aj:=lp:=NIL
         return jpk
      endif

      group := mxmlNewElement( jpk, "ZOiS")
      mxmlElementSetAttr( group, "typ", "G" )

      DEFAULT lp TO 0
      DEFAULT aj TO {0,0}

     node   := mxmlNewElement( group, 'KodKonta' )
          mxmlNewText( node,,trim(kodkonta))

     node   := mxmlNewElement( group, 'OpisKonta' )
          mxmlNewText( node,,trim(opiskonta))

     node   := mxmlNewElement( group, 'TypKonta' )
          mxmlNewText( node,,trim(typkonta))

     node   := mxmlNewElement( group, 'KodZespolu' )
          mxmlNewText( node,,trim(kodzespolu))

     node   := mxmlNewElement( group, 'OpisZespolu' )
          mxmlNewText( node,,trim(opiszespolu))

     node   := mxmlNewElement( group, 'KodKategorii' )
          mxmlNewText( node,,trim(kodkategorii))

     node   := mxmlNewElement( group, 'OpisKategorii' )
          mxmlNewText( node,,trim(opiskategorii))
          //if !empty(bown)
     node   := mxmlNewElement( group, 'BilansOtwarciaWinien' )
          mxmlNewText( node,,ltrim(tran(bown,)))
          //endif
          //if !empty(boma)
     node   := mxmlNewElement( group, 'BilansOtwarciaMa' )
          mxmlNewText( node,,ltrim(tran(boma,)))
          //endif
          //if !empty(obwn)
     node   := mxmlNewElement( group, 'ObrotyWinien' )
          mxmlNewText( node,,ltrim(tran(obwn,)))
          //endif
          //if !empty(obma)
     node   := mxmlNewElement( group, 'ObrotyMa' )
          mxmlNewText( node,,ltrim(tran(obma,)))
          //endif
          //if !empty(obwnn)
     node   := mxmlNewElement( group, 'ObrotyWinienNarast' )
          mxmlNewText( node,,ltrim(tran(obwnn,)))
          //endif
          //if !empty(obman)
     node   := mxmlNewElement( group, 'ObrotyMaNarast' )
          mxmlNewText( node,,ltrim(tran(obman,)))
          //endif
          //if !empty(salwn)
     node   := mxmlNewElement( group, 'SaldoWinien' )
          mxmlNewText( node,,ltrim(tran(salwn,)))
          //endif
          //if !empty(salma)
     node   := mxmlNewElement( group, 'SaldoMa' )
          mxmlNewText( node,,ltrim(tran(salma,)))
          //endif


     ++lp
     aj[1]+=round(salwn*100,0)
     aj[2]+=round(salma*100,0)

return jpk


func jpk_krDziennik(nr,opis_dziennika,nr_dowodu,rodzaj_dowodu,Data_op,data_dow,data_ks,operator,opis_oper,kwota)
local node

      if pcount()=0
         group := mxmlNewElement( jpk, "DziennikCtrl")
               node   := mxmlNewElement( group, 'LiczbaWierszyDziennika' )
          mxmlNewText( node,,lTrim(sTr(lp)))
               node   := mxmlNewElement( group, 'SumaKwotOperacji' )
          mxmlNewText( node,,ltrim(tran(aj/100,)))
         aj:=lp:=NIL
         return jpk
      endif

      group := mxmlNewElement( jpk, "Dziennik")
      mxmlElementSetAttr( group, "typ", "G" )

      DEFAULT lp TO 0
      DEFAULT aj TO 0

      ++lp
      aj += round(kwota*100,0)

     node   := mxmlNewElement( group, 'LpZapisuDziennika' )
          mxmlNewText( node,,lTrim(sTr(lp)))

     node   := mxmlNewElement( group, 'NrZapisuDziennika' )
          mxmlNewText( node,,alltrim(nr))

     node   := mxmlNewElement( group, 'OpisDziennika' )
          mxmlNewText( node,,alltrim(opis_dziennika))

     node   := mxmlNewElement( group, 'NrDowoduKsiegowego' )
          mxmlNewText( node,,alltrim(nr_dowodu))

     node   := mxmlNewElement( group, 'RodzajDowodu' )
          mxmlNewText( node,,alltrim(rodzaj_dowodu))

     node   := mxmlNewElement( group, 'DataOperacji' )
          mxmlNewText( node,,hb_dtoc(data_op:=if(empty(data_op),data_dow,data_op),'YYYY-MM-DD'))

     node   := mxmlNewElement( group, 'DataDowodu' )
          mxmlNewText( node,,hb_dtoc(data_dow:=if(empty(data_dow),data_op,data_dow),'YYYY-MM-DD'))

     node   := mxmlNewElement( group, 'DataKsiegowania' )
          mxmlNewText( node,,hb_dtoc(data_ks:=if(empty(data_ks),data_op,data_ks),'YYYY-MM-DD'))

     node   := mxmlNewElement( group, 'KodOperatora' )
          mxmlNewText( node,,trim(operator))

     node   := mxmlNewElement( group, 'OpisOperacji' )
          mxmlNewText( node,,trim(opis_oper))

     node   := mxmlNewElement( group, 'DziennikKwotaOperacji' )
          mxmlNewText( node,,ltrim(tran(kwota,)))

return lp

func jpk_krKontoZapis(nr,kt_wn,kwota_wn,kt_ma,kwota_ma)
local node



      if kt_wn=NIL .and. kt_ma=NIL
         group := mxmlNewElement( jpk, "KontoZapisCtrl")
               node   := mxmlNewElement( group, 'LiczbaWierszyKontoZapisj' )
          mxmlNewText( node,,lTrim(sTr(lp)))

               node   := mxmlNewElement( group, 'SumaWinien' )
          mxmlNewText( node,,ltrim(tran(kwota_wn:=aj[1]/100,)))

               node   := mxmlNewElement( group, 'SumaMa' )
          mxmlNewText( node,,ltrim(tran(kwota_ma:=aj[2]/100,)))

         aj:=NIL


         mxmlsetwrapmargin(250)
         mxmlSaveFile( tree, kt_wn:=memvar->defa+"jpk_kr"+nr+".xml" , @wscb() )
         mxmlDelete(tree)
         jpk:=tree:=group:=NIL

         //SET DATE A_SET_DAT
         return kt_wn

      endif

      DEFAULT kt_wn TO ""
      DEFAULT kt_ma TO ""
      DEFAULT kwota_wn TO 0
      DEFAULT kwota_ma TO 0

      group := mxmlNewElement( jpk, "KontoZapis")
      mxmlElementSetAttr( group, "typ", "G" )

      DEFAULT lp TO 0
      DEFAULT aj TO {0,0}

      ++lp
      aj[1] += round(kwota_wn*100,0)
      aj[2] += round(kwota_ma*100,0)

     node   := mxmlNewElement( group, 'LpZapisu' )
          mxmlNewText( node,,lTrim(sTr(lp)))

     node   := mxmlNewElement( group, 'NrZapisu' )
          mxmlNewText( node,,alltrim(nr))
          //if !empty(kt_wn)
     node   := mxmlNewElement( group, 'KodKontaWinien' )
          mxmlNewText( node,,alltrim(kt_wn))
     node   := mxmlNewElement( group, 'KwotaWinien' )
          mxmlNewText( node,,ltrim(tran(kwota_wn,)))
          //endif
          //if !empty(kt_ma)
     node   := mxmlNewElement( group, 'KodKontaMa' )
          mxmlNewText( node,,alltrim(kt_ma))
     node   := mxmlNewElement( group, 'KwotaMa' )
          mxmlNewText( node,,ltrim(tran(kwota_ma,)))
          //endif

return lp


func jpk_vatnagl(od,do,kor,waluta,firma)
local a,b,c,d,element,node


    if tree<>NIL
      mxmlDelete(tree)
      tree:=NIL
    endif

    //SET DATE FORMAT TO "YYYY-MM-DD"
    tree:= mxmlNewXML()
    jpk := mxmlNewElement( tree, 'JPK' )
    
    dekl := do >= 0d20201001
    ver := if(do>=0d20220101,2,1)

if dekl
    
    if empty(firma)
       hb_HAutoAdd(firma := {"KodUrzedu" => Trim(memvar->firma_Urz)}, .t.)
       if __mvExist("firma_DataUr")
          firma["etd:NIP"] := Trim(memvar->firma_NIP)
          firma["etd:ImiePierwsze"] := Trim(memvar->firma_imie)
          firma["etd:Nazwisko"] := Trim(memvar->firma_nazwisko)
          firma["etd:DataUrodzenia"] := hb_dtoc(memvar->firma_DataUr, "YYYY-MM-DD")
       else
          firma["NIP"] := Trim(memvar->firma_NIP)
          firma["PelnaNazwa"] := Trim(memvar->firma_pelnaz)
       endif
       if __mvExist('firma_email')
          firma["Email"] := trim(memvar->firma_email) 
       endif
       if __mvExist('firma_tel')
          firma["Telefon"] := trim(memvar->firma_tel)
       endi
    endif 


    mxmlElementSetAttr( jpk, "xmlns:etd", if(ver=2,"http://crd.gov.pl/xml/schematy/dziedzinowe/mf/2021/06/08/eD/DefinicjeTypy/","http://crd.gov.pl/xml/schematy/dziedzinowe/mf/2020/03/11/eD/DefinicjeTypy/"))
    mxmlElementSetAttr( jpk, HB_EOL()+chr(9)+"xmlns", DD_NAMESPACE)
    element   := mxmlNewElement( jpk, 'Naglowek' )

     node := mxmlNewElement( element, "KodFormularza")

      mxmlElementSetAttr( node, "kodSystemowy", "JPK_V7M ("+STR(DD_WARIANT,1)+")" )

      mxmlElementSetAttr( node, "wersjaSchemy", DD_SCHEMA )
      mxmlNewText( node,, "JPK_VAT")

     node := mxmlNewElement( element, "WariantFormularza")
      mxmlNewText( node,, STR(DD_WARIANT,1))

     node := mxmlNewElement( element, "DataWytworzeniaJPK")
      mxmlNewText( node,, stuff(HB_TSTOSTR(HB_TSTOUTC(hb_DateTime())),11,1,'T')+'Z')

#ifdef A_STOPKA
     node := mxmlNewElement( element, "NazwaSystemu")   
      mxmlNewText( node,, A_STOPKA )
#endif

     node := mxmlNewElement( element, "CelZlozenia")
      mxmlElementSetAttr( node, "poz", "P_7" )
      mxmlNewText( node,,if(empty(kor),'1','2'))

     node := mxmlNewElement( element, "KodUrzedu")
      mxmlNewText( node,, firma["KodUrzedu"])

     node := mxmlNewElement( element, "Rok")
      mxmlNewText( node,, str(year(do),4) )

     node := mxmlNewElement( element, "Miesiac")
      mxmlNewText( node,, str(month(do),2) )

     group   := mxmlNewElement( jpk, 'Podmiot1' )

    mxmlElementSetAttr( group, "rola", "Podatnik" )

     element := mxmlNewElement( group, if (hb_HHasKey(firma,'etd:DataUrodzenia'), 'OsobaFizyczna', 'OsobaNiefizyczna'))

     hb_HEval(firma, {|h,v,i| mxmlNewText( mxmlNewElement( element, h),, Tran(v,)) }, 2)

else
    mxmlElementSetAttr( jpk, "xmlns:etd", "http://crd.gov.pl/xml/schematy/dziedzinowe/mf/2016/01/25/eD/DefinicjeTypy/")
    mxmlElementSetAttr( jpk, HB_EOL()+chr(9)+"xmlns", D_NAMESPACE)
    element   := mxmlNewElement( jpk, 'Naglowek' )

     node := mxmlNewElement( element, "KodFormularza")
      mxmlElementSetAttr( node, "kodSystemowy", "JPK_VAT ("+STR(D_WARIANT,1)+")" )
      mxmlElementSetAttr( node, "wersjaSchemy", D_SCHEMA )
      mxmlNewText( node,, "JPK_VAT")

     node := mxmlNewElement( element, "WariantFormularza")
      mxmlNewText( node,, STR(D_WARIANT,1))

     node := mxmlNewElement( element, "CelZlozenia")
#if D_WARIANT == 2
      mxmlNewText( node,, if(empty(kor),"1","2"))
#else
      mxmlNewText( node,,lTrim(sTr(kor)))
#endif

     node := mxmlNewElement( element, "DataWytworzeniaJPK")
      mxmlNewText( node,, stuff(HB_TSTOSTR(HB_TSTOUTC(hb_DateTime())),11,1,'T')+'Z')

     node := mxmlNewElement( element, "DataOd")
      mxmlNewText( node,, hb_dtoc(od,'YYYY-MM-DD') )

     node := mxmlNewElement( element, "DataDo")
      mxmlNewText( node,, hb_dtoc(do,'YYYY-MM-DD') )

#if D_WARIANT == 3
     node := mxmlNewElement( element, "NazwaSystemu")   
      mxmlNewText( node,, A_STOPKA )
#else
     node := mxmlNewElement( element, "DomyslnyKodWaluty")

DEFAULT waluta TO 'PLN'
      mxmlNewText( node,, waluta )

     node := mxmlNewElement( element, "KodUrzedu")
      mxmlNewText( node,, Trim(memvar->firma_Urz) )
#endif

    element := group   := mxmlNewElement( jpk, 'Podmiot1' )

#if D_WARIANT == 2
     element := mxmlNewElement( group, 'IdentyfikatorPodmiotu' )

      node := mxmlNewElement( element, "etd:NIP")
       mxmlNewText( node,, Trim(memvar->firma_NIP) )

      node := mxmlNewElement( element, "etd:PelnaNazwa")
       mxmlNewText( node,, Trim(memvar->firma_PelNaz) )
#endif

#if D_WARIANT == 3
      node := mxmlNewElement( element, "NIP")
       mxmlNewText( node,, Trim(memvar->firma_NIP) )

      node := mxmlNewElement( element, "PelnaNazwa")
       mxmlNewText( node,, Trim(memvar->firma_PelNaz) )
   
    if !empty(memvar->firma_email)
	  
      node := mxmlNewElement( element, "Email")
       mxmlNewText( node,, trim(memvar->firma_email) )
    endif
#else
      if !empty(memvar->firma_REGON)
      node := mxmlNewElement( element, "etd:REGON")
       mxmlNewText( node,, trim(memvar->firma_REGON) )
      endif
     element := mxmlNewElement( group, 'AdresPodmiotu' )

      node := mxmlNewElement( element, "KodKraju")
       mxmlNewText( node,, "PL" )

      node := mxmlNewElement( element, "Wojewodztwo")
       mxmlNewText( node,, trim(memvar->firma_Woj) )

      node := mxmlNewElement( element, "Powiat")
       mxmlNewText( node,, trim(memvar->firma_Pow) )

      node := mxmlNewElement( element, "Gmina")
       mxmlNewText( node,, Trim(memvar->firma_Gmin) )

      if !empty(memvar->firma_ul)
      node := mxmlNewElement( element, "Ulica")
       mxmlNewText( node,, Trim(memvar->firma_Ul) )
      endif

      node := mxmlNewElement( element, "NrDomu")
       mxmlNewText( node,, TRim(memvar->firma_Dom) )

      node := mxmlNewElement( element, "Miejscowosc")
       mxmlNewText( node,, Trim(memvar->firma_miasto) )

      node := mxmlNewElement( element, "KodPocztowy")
       mxmlNewText( node,, left(memvar->firma_poczta,6) )

      node := mxmlNewElement( element, "Poczta")
       mxmlNewText( node,, trim(SubStr(memvar->firma_poczta,8)) )
#endif
endif

      groupd := NIL

      group := NIL

return jpk

func jpk_vatdeklaracja(wariant, deklaracja, na, ar)
     local a,b,c,d,element,node

     if dekl

     if empty(groupd)
        groupd := mxmlNewElement( jpk, 'Deklaracja' )
     endif

     if !empty(wariant) .and. (valtype(deklaracja) = 'H' .or. valtype(ar) = 'A')
        a := {=>}
        hb_hAutoAdd(a,.t.)
        hb_hKeepOrder(a,.f.)
        if valtype(deklaracja) = 'H'
          hb_heval(deklaracja,{|k,v,i|a[if(len(k)>4,k,k+' ')]:=v})
        endif
        deklaracja := a
        a := NIL
        if !empty(ar)
          aeval( ar, {|x,y| y:='P_'+lTrim(sTr(y + D_SPRZEDOFFSET))+' ' ,if(empty(x),, deklaracja[y] := x)})
        endif

      deklaracja['P_37 '] := hb_hgetdef(deklaracja,'P_10 ',0) +;
                             hb_hgetdef(deklaracja,'P_11 ',0) +;
                             hb_hgetdef(deklaracja,'P_13 ',0) +;
                             hb_hgetdef(deklaracja,'P_15 ',0) +;
                             hb_hgetdef(deklaracja,'P_17 ',0) +;
                             hb_hgetdef(deklaracja,'P_19 ',0) +;
                             hb_hgetdef(deklaracja,'P_21 ',0) +;
                             hb_hgetdef(deklaracja,'P_22 ',0) +;
                             hb_hgetdef(deklaracja,'P_23 ',0) +;
                             hb_hgetdef(deklaracja,'P_25 ',0) +;
                             hb_hgetdef(deklaracja,'P_27 ',0) +;
                             hb_hgetdef(deklaracja,'P_29 ',0) +;
                             hb_hgetdef(deklaracja,'P_31 ',0)

      deklaracja['P_38 '] := hb_hgetdef(deklaracja,'P_16 ',0) +;
                             hb_hgetdef(deklaracja,'P_18 ',0) +;
                             hb_hgetdef(deklaracja,'P_20 ',0) +;
                             hb_hgetdef(deklaracja,'P_24 ',0) +;
                             hb_hgetdef(deklaracja,'P_26 ',0) +;
                             hb_hgetdef(deklaracja,'P_28 ',0) +;
                             hb_hgetdef(deklaracja,'P_30 ',0) +;
                             hb_hgetdef(deklaracja,'P_32 ',0) +;
                             hb_hgetdef(deklaracja,'P_33 ',0) +;
                             hb_hgetdef(deklaracja,'P_34 ',0) -;
                             hb_hgetdef(deklaracja,'P_35 ',0) -;
                             hb_hgetdef(deklaracja,'P_36 ',0)

      deklaracja['P_48 '] := hb_hgetdef(deklaracja,'P_39 ',0) +;
                             hb_hgetdef(deklaracja,'P_41 ',0) +;
                             hb_hgetdef(deklaracja,'P_43 ',0) +;
                             hb_hgetdef(deklaracja,'P_44 ',0) +;
                             hb_hgetdef(deklaracja,'P_45 ',0) +;
                             hb_hgetdef(deklaracja,'P_46 ',0) +;
                             hb_hgetdef(deklaracja,'P_47 ',0)

      a := max(0,deklaracja['P_38 '] - deklaracja['P_48 '])
      b := hb_hgetdef(deklaracja,'P_49 ',0)
      if b > a
         deklaracja['P_49 '] := 0
         deklaracja['P_52 '] := hb_hgetdef(deklaracja,'P_52 ',0) + b - a
      endif

      deklaracja['P_51 '] := max(0,a:=deklaracja['P_38 '] -;
                            deklaracja['P_48 '] -;
                            hb_hgetdef(deklaracja,'P_49 ',0) -;
                            hb_hgetdef(deklaracja,'P_50 ',0))

      b := hb_hgetdef(deklaracja,'P_52 ',0) - a
      if b > 0
         deklaracja['P_53 '] := b
         deklaracja['P_62 '] := max(0,b - hb_hgetdef(deklaracja,'P_54 ',0) - hb_hgetdef(deklaracja,'P_60 ',0))
      endif

     //group := groupd := mxmlNewElement( jpk, 'Deklaracja' )
     //group := groupd

     element := mxmlNewElement( groupd, 'Naglowek' )

     node := mxmlNewElement( element, 'KodFormularzaDekl')
          mxmlElementSetAttr( node, "kodSystemowy", "VAT-7 ("+wariant+")")
          mxmlElementSetAttr( node, "kodPodatku", "VAT")
          mxmlElementSetAttr( node, "rodzajZobowiazania", "Z")
          mxmlElementSetAttr( node, "wersjaSchemy", DD_SCHEMA)
          mxmlNewText( node,, "VAT-7")
     node := mxmlNewElement( element, "WariantFormularzaDekl")
          mxmlNewText( node,, wariant)

     element := mxmlNewElement( groupd, "PozycjeSzczegolowe")

     hb_HEval(deklaracja, {|h,v,i|mxmlNewText( mxmlNewElement( element, trim(h)),, tran(v,))})

     element := mxmlNewElement( groupd, "Pouczenia")
          mxmlNewText( element,, "1")

     endif
     endif
     if !empty(na)
       mxmlsetwrapmargin(250)
       mxmlSaveFile( tree, na:=memvar->defa+"jpkvdek"+na+".xml" , @wscb() )
       mxmlDelete(tree)
       tree:=NIL
       //SET DATE A_SET_DAT
     endif

return deklaracja

func jpk_vath(a)

if empty(a)
  a:=hb_hash("TypDokumentu",,"GTU_01",,"GTU_02",,"GTU_03",,"GTU_04",,;
  "GTU_05",,"GTU_06",,"GTU_07",,"GTU_08",,"GTU_09",,"GTU_10",,"GTU_11",,;
  "GTU_12",,"GTU_13",,"WSTO_EE",, "IED",, "TP",, "TT_WNT",,;
  "TT_D",, "MR_T",, "MR_UZ",, "SW",,"EE",,"TP",,"TT_WNT",,"TT_D",,"MR_T",,"MR_UZ",,;
  "I_42",,"I_63",,"B_SPV",,"B_SPV_DOSTAWA",,"B_MPV_PROWIZJA",,"MPP",,;
  "KorektaPodstawyOpodt",, "TerminPlatnosci",, "DataZaplaty",,;
  "K_10",,"K_11",,"K_12",,"K_13",,"K_14",,"K_15",,;
  "K_16",,"K_17",,"K_18",,"K_19",,"K_20",,"K_21",,"K_22",,"K_23",,"K_24",,;
  "K_25",,"K_26",,"K_27",,"K_28",,"K_29",,"K_30",,"K_31",,"K_32",,"K_33",,;
  "K_34",,"K_35",,"K_36",,"SprzedazVAT_Marza",)
else
  a:=hb_hash("DokumentZakupu",,"MPP",,"IMP",,"K_40",,"K_41",,"K_42",,;
  "K_43",,"K_44",,"K_45",,"K_46",,"K_47",,"ZakupVAT_Marza",)
endif
  //hb_hautoAdd(hsempty,.t.)
  //hb_hautoadd(hzempty,.t.)
return a

func jpk_vatsprzed(da,ds,nd,na,ad,nip,ar,arn) //firmy i dm
local a,b,c,d,element,node,i,fp

    if empty(group)
       if dekl
          group := mxmlNewElement( jpk, 'Ewidencja' )
          aj:=array(36 - D_SPRZEDOFFSET)
       else
          group := jpk
          aj:=array(39 - D_SPRZEDOFFSET)
       endif
      lp:=0
      afill(aj,0)
    endif

    if da=NIL
       ds:=aj
       //if lp>0
         element   := mxmlNewElement( group, 'SprzedazCtrl' )

         node := mxmlNewElement( element, 'LiczbaWierszySprzedazy' )
         mxmlNewText( node,,ltrim(tran(lp,)))

         node := mxmlNewElement( element, 'PodatekNalezny' )
if dekl 
         //                             K_16, K_18,  K_20,  K_24,  K_26,  K_28,  K_30,  K_32,  K_33 i K_34 - K_35 - K_36
         //                               16    18     20     24     26     28     30     32     33     34     35     36 
         mxmlNewText( node,,ltrim(tran((aj[7]+aj[9]+aj[11]+aj[15]+aj[17]+aj[19]+aj[21]+aj[23]+aj[24]+aj[25]-aj[26]-aj[27])/100,)) )
else
         //                             K_16, K_18,  K_20,  K_24,  K_26,  K_28,  K_30,  K_33,  K_35 i K_36 + K_37 - K_38 - K_39
         //                               16    18     20     24     26     28     30     33     35     36     37     38     39
         mxmlNewText( node,,ltrim(tran((aj[7]+aj[9]+aj[11]+aj[15]+aj[17]+aj[19]+aj[21]+aj[24]+aj[26]+aj[27]+aj[28]-aj[29]-aj[30])/100,)) )
endif         
       //endif

       IF empty(na)
          na := array(len(aj))
       elseif len(na) < len(aj)
          asize(na,len(aj))
       endif

       aeval(aj,{|x,y|na[y]:=int(Round(x,-2)/100),aj[y]/=100}) // 0,49999999999 zaokr najpierw do 0,50!

if dekl 
//       if len(na) < 38 - D_SPRZEDOFFSET
//          asize(na, 38 - D_SPRZEDOFFSET)
//       endif
//       na[28]:=na[1]+na[2]+na[4]+na[6]+na[8]+na[10]+na[12]+na[14]+na[16]+na[18]+na[20]+na[22]
       da:= na[7]+na[9]+na[11]+na[15]+na[17]+na[19]+na[21]+na[23]+na[24]+na[25]-na[26]-na[27]
//       da:=na[29]
else
       da:=na[7]+na[9]+na[11]+na[15]+na[17]+na[19]+na[21]+na[24]+na[26]+na[27]+na[28]-na[29]-na[30]
endif
     lp:=0
     aj:=array(8)
     afill(aj,0)

     if !empty(nd)
       mxmlsetwrapmargin(250)
       mxmlSaveFile( tree, nd:=memvar->defa+if(dekl,"jpkvdek","jpk_vat")+nd+".xml" , @wscb() )
       mxmlDelete(tree)
       tree:=NIL
       //SET DATE A_SET_DAT
     endif
     return na

    endif

    if !empty(nip) .and. isalpha(nip) .and. len(strtran(nip,' '))<10
       nip:='brak'
    endif


#command DEFAULT <x> TO <y> => IF empty(<x>);<x>:=<y>;ENDIF
    DEFAULT ds  TO da
    DEFAULT nd  TO 'brak'
    DEFAULT na  TO 'brak'
    DEFAULT nip TO 'brak'

    element   := mxmlNewElement( group, 'SprzedazWiersz' )

#if D_WARIANT == 2
     mxmlElementSetAttr( element, "typ", "G" )
#endif
     node := mxmlNewElement( element, 'LpSprzedazy' )
     mxmlNewText( node,,ltrim(tran(++lp,)))

    if dekl .and. len(trim(nip))>10 .and. nip>='A'
     node := mxmlNewElement( element, 'KodKrajuNadaniaTIN')
     mxmlNewText( node,,left(nip,2))
     nip := alltrim(SubStr(nip,3))
    endif 

     node := mxmlNewElement( element, 'NrKontrahenta' )
     mxmlNewText( node,,strtran(trim(nip),'-') )

     node := mxmlNewElement( element, 'NazwaKontrahenta' )
     mxmlNewText( node,,trim(na) )

    if !dekl
      DEFAULT ad TO 'brak'
      node := mxmlNewElement( element, 'AdresKontrahenta' )
      mxmlNewText( node,,trim(ad) )
    endif 

     node := mxmlNewElement( element, 'DowodSprzedazy' )
     mxmlNewText( node,,trim(nd))

     node := mxmlNewElement( element, 'DataWystawienia' )
     mxmlNewText( node,,hb_dtoc(da,'YYYY-MM-DD') )

     if da<>ds
        node := mxmlNewElement( element, 'DataSprzedazy' )
        mxmlNewText( node,,hb_dtoc(ds,'YYYY-MM-DD') )
     endif

//     if dekl .and. !empty(td)
//        node := mxmlNewElement( element, 'TypDokumentu')
//        mxmlNewText( node,,trim(td))
//     endif


     if dekl
        if empty(arn)
             b := ar
             //    32,33,34,35,36,37,38,39
             //      ,  ,  ,32,33,34,35,36  //trans
             c := {  ,  ,  ,23,24,25,26,27} //trans
        else
             b := arn
             c := NIL
        endif
      else
          if empty(ar)
             b := arn
             //    32,33,34,35,36
             //   {35,36,37,38,39} //trans
             c := {26,27,28,29,30} //trans
          else
             b := ar
             c := NIL
          endif
      endif
       d:={}

     if valtype(b)='H'
       fp := hb_hgetdef(b,'TypDokumentu',NIL) == 'FP'
       hb_heval(b,{|k,v,n|;
         if(k='K_'.and.v<>NIL,(;
             n:=val(SubStr(k,3)),;
             n:=if(c=NIL.or.n<32,n-D_SPRZEDOFFSET,n:=c[n-31]),;
             if(n=NIL,;
               v:=NIL,;
             );
           ),;
           n:=NIL;
         ),;
         if(n=NIL,,(;
           k:='K_'+str(n+D_SPRZEDOFFSET,2),;
           if(fp,,(;
             aj[n]+=round(100*v,0),;
             if(len(d)<n,;
               asize(d,n),;
             ),;
             d[n]:=v;
           ));
         )),;
         if(v=NIL,,;
            mxmlNewText(mxmlNewElement(element,k),,ltrim(tran(v,)));
         );
       })
     else
       aeval(b,{|x,i,a|;
         if(x=NIL,,(;
            a:=if(c=NIL.or. i <= 31-D_SPRZEDOFFSET ,i,c[i-(31-D_SPRZEDOFFSET)]),;
            if(a=NIL,,(;
              if(len(d)<a,;
                asize(d,a),;
              ),;
              x:=round(100*x,0),;
              aj[a]+=x,;
              d[a]:=x/=100,;
              mxmlNewText(mxmlNewElement( element, 'K_'+str(a+D_SPRZEDOFFSET,2) ),,ltrim(tran(x,)));
            ));
         ));
       })

/*****************
     for i:=1 to len(b)
       if b[i]=NIL
          LOOP
       endif

       a := if(c=NIL.or. i <= 31-D_SPRZEDOFFSET ,i,c[i-(31-D_SPRZEDOFFSET)])

       IF a = NIL
          LOOP
       endif

       if len(d)<a
          asize(d,a)
       endif
       d[a]:=round(100*b[i],0)

       aj[a]+=d[a]
       d[a]/=100

       node := mxmlNewElement( element, 'K_'+str(a+D_SPRZEDOFFSET,2) )
       mxmlNewText( node,,ltrim(tran(d[a],)) )
     next i
******************/
     endif
return d

func jpk_vatzakup(da,ds,nd,na,ad,nip,ar,arn) //firmy i dm
local a,b,c,d,element,node,i

    DEFAULT nd  TO ''

      if da=NIL


        //if lp>0

         element   := mxmlNewElement( group, 'ZakupCtrl' )

          node := mxmlNewElement( element, 'LiczbaWierszyZakupow' )
          mxmlNewText( node,,ltrim(tran(lp,)))


          node := mxmlNewElement( element, 'PodatekNaliczony' )
          mxmlNewText( node,,ltrim(tran((aj[2]+aj[4]+aj[5]+aj[6]+aj[7]+aj[8])/100,)) )

        //endif
         ds:=aj

        if dekl
         if empty(na)
            na:= array(len(aj) + D_ZAKOFFSET - D_SPRZEDOFFSET)
         elseif len(na) < len(aj) + D_ZAKOFFSET - D_SPRZEDOFFSET
            asize(na,len(aj) + D_ZAKOFFSET - D_SPRZEDOFFSET)
         endif

         aeval(aj,{|x,y|na[y + D_ZAKOFFSET - D_SPRZEDOFFSET]:=int(Round(x,-2)/100),aj[y]/=100})
         da:= na[2+ D_ZAKOFFSET - D_SPRZEDOFFSET]+na[4+ D_ZAKOFFSET - D_SPRZEDOFFSET]+na[5+ D_ZAKOFFSET - D_SPRZEDOFFSET]+na[6+ D_ZAKOFFSET - D_SPRZEDOFFSET]+na[7+ D_ZAKOFFSET - D_SPRZEDOFFSET]+na[8+ D_ZAKOFFSET - D_SPRZEDOFFSET]
         //na[48 - D_SPRZEDOFFSET] := da
         //na[51 - D_SPRZEDOFFSET] := max(0,na[38 - D_SPRZEDOFFSET] - if(na[39 - D_SPRZEDOFFSET]=NIL, 0, na[39 - D_SPRZEDOFFSET]) - na[48 - D_SPRZEDOFFSET] - if(na[49 - D_SPRZEDOFFSET]=NIL,0,na[49 - D_SPRZEDOFFSET]) - if(na[50 - D_SPRZEDOFFSET]=NIL,0,na[50 - D_SPRZEDOFFSET]))

         ar:=array(len(aj))
         acopy(na,ar, 1 + D_ZAKOFFSET - D_SPRZEDOFFSET, len(aj))
        else
         if empty(na)
            na:= array(len(aj))
         endif
         aeval(aj,{|x,y|na[y]:=Round(x,-2)/100,aj[y]/=100})
         da:=na[2]+na[4]+na[5]+na[6]+na[7]+na[8]
         ar := na
        endif 

     if !empty(nd)
       mxmlsetwrapmargin(250)
       mxmlSaveFile( tree, nd:=memvar->defa+if(dekl,"jpkvdek","jpk_vat")+nd+".xml" , @wscb() )
       mxmlDelete(tree)
       tree:=NIL
       //SET DATE A_SET_DAT
     endif

         return ar

      endif

    if !empty(nip) .and. isalpha(nip) .and. len(strtran(nip,' '))<10
       nip:='brak'
    endif

    DEFAULT ds  TO da
    DEFAULT na  TO 'brak'
    DEFAULT nip TO 'brak'

     //d:=set(_SET_DATEFORMAT,'YYYY-MM-DD')

    element   := mxmlNewElement( group, 'ZakupWiersz' )

#if D_WARIANT == 2
    mxmlElementSetAttr( element, "typ", "G" )
#endif
     node := mxmlNewElement( element, 'LpZakupu' )
     mxmlNewText( node,,ltrim(tran(++lp,)))

     if dekl .and. len(trim(nip))>10 .and. nip>='A'
          node := mxmlNewElement( element, 'KodKrajuNadaniaTIN')
          mxmlNewText( node,,left(nip,2))
          nip := alltrim(SubStr(nip,3))
     endif 
     
     node := mxmlNewElement( element, 'NrDostawcy' )
     mxmlNewText( node,,strtran(trim(nip),'-') )

     node := mxmlNewElement( element, 'NazwaDostawcy' )
     mxmlNewText( node,,trim(na) )

    if !dekl
     DEFAULT ad  TO 'brak'
     node := mxmlNewElement( element, 'AdresDostawcy' )
     mxmlNewText( node,,trim(ad) )
    endif 

     node := mxmlNewElement( element, 'DowodZakupu' )
     mxmlNewText( node,,trim(nd))

     node := mxmlNewElement( element, 'DataZakupu' )
     mxmlNewText( node,,hb_dtoc(da,'YYYY-MM-DD') )

     if da<>ds
       node := mxmlNewElement( element, 'DataWplywu' )
       mxmlNewText( node,,hb_dtoc(ds,'YYYY-MM-DD') )
     endif

//     if dekl .and. !empty(dz)
//          node := mxmlNewElement( element, 'DokumentZakupu')
//          mxmlNewText( node,,trim(dz))
//     endif

     if dekl
        c := D_ZAKOFFSETnew //output
        if empty(arn)
             b := ar
             a := D_ZAKOFFSETold // input
        else
             b := arn
             a := D_ZAKOFFSETnew
        endif
      else
        c := D_ZAKOFFSETold //output
          if empty(ar)
             b := arn
             a := D_ZAKOFFSETnew
          else
             b := ar
             a := D_ZAKOFFSETold
          endif
      endif
     if valtype(b)='H'
       d:={}
       hb_heval(b,{|k,v,n|if(k='K_'.and.v<>NIL,(n:=val(SubStr(k,3))-a,aj[n]+=round(100*v,0),if(len(d)<n,asize(d,n),),d[n]:=v,k:='K_'+str(n+c,2)),),;
       if(v=NIL,,mxmlNewText(mxmlNewElement( element, k ),,ltrim(tran(v,))))})
     else
       aeval(b,{|x,i|;
         if(x=NIL,,(;
            x:=round(100*x,0),aj[i]+=x,b[i]:=x/=100,;
            mxmlNewText(mxmlNewElement( element, 'K_'+str(i+c,2) ),,ltrim(tran(b[i],)));
         ));
       },1,8)

/**********
     for i:=1 to min(8,len(b))
       if b[i]=NIL
          LOOP
       endif

       a:=round(100*b[i],0)
       aj[i]+=a
       b[i]:=a/100

       if !empty(b[i])
         node := mxmlNewElement( element, 'K_'+str(i+c,2) )
         mxmlNewText( node,,ltrim(tran(b[i],)) )
       endif
     next i
***********/
        return b
     endif

return d

func jpk_fanagl(od, do, waluta)
local a,b,c,d,element,node

DEFAULT waluta TO 'PLN'

    if tree<>NIL
      a:=memvar->defa+"jpk_fa"+dtos(od)+waluta+".xml"
      mxmlsetwrapmargin(250)
      mxmlSaveFile( tree, a , @wscb() )
      mxmlDelete(tree)
      tree:=NIL
      //SET DATE A_SET_DAT
      return a
    endif

    //set date format to "YYYY-MM-DD"
    tree:= mxmlNewXML()
    jpk := mxmlNewElement( tree, 'JPK' )

    mxmlElementSetAttr( jpk, "xmlns:etd", D_FASCHEMA)
    mxmlElementSetAttr( jpk, HB_EOL()+chr(9)+"xmlns", D_FANAMESPACE)
    element   := mxmlNewElement( jpk, 'Naglowek' )

     node := mxmlNewElement( element, "KodFormularza")
      mxmlElementSetAttr( node, "kodSystemowy", "JPK_FA ("+str(D_FAVARIANT,1)+")" )
      mxmlElementSetAttr( node, "wersjaSchemy", "1-0" )
      mxmlNewText( node,, "JPK_FA")

     node := mxmlNewElement( element, "WariantFormularza")
      mxmlNewText( node,, str(D_FAVARIANT,1))

     node := mxmlNewElement( element, "CelZlozenia")
      mxmlNewText( node,, "1")

     //d:=set(_SET_DATEFORMAT,'YYYY-MM-DD')

     node := mxmlNewElement( element, "DataWytworzeniaJPK")
      mxmlNewText( node,, stuff(HB_TSTOSTR(HB_TSTOUTC(hb_DateTime())),11,1,'T')+'Z')

     node := mxmlNewElement( element, "DataOd")
      mxmlNewText( node,, hb_dtoc(od,'YYYY-MM-DD') )

     node := mxmlNewElement( element, "DataDo")
      mxmlNewText( node,, hb_dtoc(do,'YYYY-MM-DD') )

      //set(_SET_DATEFORMAT,d)
#if D_FAVARIANT == 1
     node := mxmlNewElement( element, "DomyslnyKodWaluty")
      mxmlNewText( node,, waluta )
#endif

     node := mxmlNewElement( element, "KodUrzedu")
      mxmlNewText( node,, memvar->firma_Urz )

    group   := mxmlNewElement( jpk, 'Podmiot1' )

     element := mxmlNewElement( group, 'IdentyfikatorPodmiotu' )

      node := mxmlNewElement( element, "NIP")
       mxmlNewText( node,, Trim(memvar->firma_NIP) )

      node := mxmlNewElement( element, "PelnaNazwa")
       mxmlNewText( node,, Trim(memvar->firma_pelnaz) )

#if D_FAVARIANT == 1
      if !empty(memvar->firma_REGON)
      node := mxmlNewElement( element, "REGON")
       mxmlNewText( node,, trim(memvar->firma_REGON) )
      endif
#endif
     element := mxmlNewElement( group, 'AdresPodmiotu' )

      node := mxmlNewElement( element, "etd:KodKraju")
       mxmlNewText( node,, "PL" )

      node := mxmlNewElement( element, "etd:Wojewodztwo")
       mxmlNewText( node,, trim(memvar->firma_Woj) )

      node := mxmlNewElement( element, "etd:Powiat")
       mxmlNewText( node,, trim(memvar->firma_Pow ))

      node := mxmlNewElement( element, "etd:Gmina")
       mxmlNewText( node,, trim(memvar->firma_Gmin ))

      if !empty(memvar->firma_ul)
      node := mxmlNewElement( element, "etd:Ulica")
       mxmlNewText( node,, trim(memvar->firma_Ul ))
      endif

      node := mxmlNewElement( element, "etd:NrDomu")
       mxmlNewText( node,, trim(memvar->firma_Dom ))

//      if !empty(memvar->firma_lok)
//      node := mxmlNewElement( element, "etd:NrLokalu")
//       mxmlNewText( node,, trim(memvar->firma_lok ))
//      endif

      node := mxmlNewElement( element, "etd:Miejscowosc")
       mxmlNewText( node,, trim(memvar->firma_miasto) )

      node := mxmlNewElement( element, "etd:KodPocztowy")
       mxmlNewText( node,, left(memvar->firma_poczta,6) )

#if D_FAVARIANT == 1
      node := mxmlNewElement( element, "etd:Poczta")
       mxmlNewText( node,, trim(SubStr(memvar->firma_poczta,8)) )
#endif
    lp:=aj:=0

return jpk

func jpk_fafv(ht)
local a,b,c,d,element,node,i

  if ht=NIL
    if !empty(lp)

    element   := mxmlNewElement( jpk, 'FakturaCtrl' )

     node := mxmlNewElement( element, 'LiczbaFaktur' )
     mxmlNewText( node,,ltrim(tran(lp,)))

     node := mxmlNewElement( element, 'WartoscFaktur' )
     mxmlNewText( node,,ltrim(tran(round(hb_hgetdef(aj,'P_15',0),2),)))
    endif

     a:=lp
     ht:=aj

     aj:=lp:=NIL

     return a

  endif

  if empty(lp)
    lp:=0
    hb_hAutoAdd(aj:={=>},.t.)
  endif

     //d:=set(_SET_DATEFORMAT,'YYYY-MM-DD')

    element   := mxmlNewElement( jpk, 'Faktura' )

#if D_FAVARIANT == 1
    mxmlElementSetAttr( element, "typ", "G" )
#endif
a:={"KodWaluty","P_1", "P_2A", "P_3A", "P_3B", "P_3C", "P_3D", "P_4A", "P_4B", "P_5A", "P_5B", "P_6",;
    "P_13_1", "P_14_1", "P_14_1W", "P_13_2", "P_14_2", "P_14_2W", "P_13_3", "P_14_3",;
    "P_14_3W", "P_13_4", "P_14_4", "P_14_4W", "P_13_5", "P_14_5", "P_13_6", "P_13_7",;
    "P_15", "P_16", "P_17", "P_18", "P_18A", "P_19", "P_19A", "P_19B", "P_19C", "P_20",;
    "P_20A", "P_20B", "P_21", "P_21A", "P_21B", "P_21C", "P_22", "P_22A", "P_22B", "P_22C",;
    "P_23", "P_106E_2", "P_106E_3", "P_106E_3A", "RodzajFaktury", "PrzyczynaKorekty",;
    "NrFaKorygowanej", "OkresFaKorygowanej", "NrFaZaliczkowej"}

     aeval(a,{|k,v|v:=hb_hgetdef(ht,k,NIL),;
                      if(valtype(v)='N',aj[k]:=hb_hgetdef(aj,k,0)+v,),;
                      v:=if(v=NIL,'',ltrim(tran(if(valtype(v)='L',if(v,'true','false'),v),))),;
                      if(v=='',,mxmlNewText(mxmlNewElement( element, k ),,v))})
     //hb_heval(ht,{|k,v,n|if(valtype(v)='N',aj[k]:=hb_hgetdef(aj,k,0)+v,),mxmlNewText(mxmlNewElement( element, k ),,ltrim(tran(v,)))})

return ++lp

func jpk_fastawki(st)
#if D_FAVARIANT == 1
local element   := mxmlNewElement( jpk, 'StawkiPodatku' )
    aeval(st,{|x,n|mxmlNewText(mxmlNewElement( element, 'Stawka'+str(n,1) ),,ltrim(tran(val(x)/100,))) } )
#endif
return len(st)

func jpk_faWiersz(ht)
local a,b,c,d,element,node,i

  if ht=NIL
    if !empty(lp)

    element   := mxmlNewElement( jpk, 'FakturaWierszCtrl' )

     node := mxmlNewElement( element, 'LiczbaWierszyFaktur' )
     mxmlNewText( node,,ltrim(tran(lp,)))

     node := mxmlNewElement( element, 'WartoscWierszyFaktur' )
     mxmlNewText( node,,ltrim(tran(round(hb_hgetdef(aj,'P_11',0),2),)))
    endif

     ht:=aj
     a:=lp

     lp:=aj:=NIL

     return a

  endif

  if empty(lp)
    lp:=0
    hb_hAutoAdd(aj:={=>},.t.)
  endif

    element   := mxmlNewElement( jpk, 'FakturaWiersz' )

#if D_FAVARIANT == 1
    mxmlElementSetAttr( element, "typ", "G" )
#endif
    a:={"P_2B", "P_7", "P_8A", "P_8B", "P_9A", "P_9B", "P_10", "P_11", "P_11A", "P_12", "P_12_XII"}

    aeval(a,{|k,v|v:=hb_hgetdef(ht,k,NIL),if(valtype(v)='N',aj[k]:=hb_hgetdef(aj,k,0)+v,),if(v=NIL,,mxmlNewText(mxmlNewElement( element, k ),,ltrim(tran(if(valtype(v)='L',if(v,'true','false'),v),))))})
    // hb_heval(ht,{|k,v,n|if(valtype(v)='N',aj[k]:=hb_hgetdef(aj,k,0)+v,),mxmlNewText(mxmlNewElement( element, k ),,ltrim(tran(v,)))})

return ++lp


func jpk_magnagl(od,do, waluta)
local a,b,c,d,element,node


    if tree<>NIL
      if empty(od) .or. empty(do)
         a:=memvar->defa+"jpk_mag"+alltrim(mag_biez)+".xml"
      else
         a:=memvar->defa+"jpk_mag"+alltrim(mag_biez)+'_'+dtos(od)+'_'+SubStr(dtos(do),5)+".xml"
      endif
      mxmlsetwrapmargin(250)
      mxmlSaveFile( tree, a , @wscb() )
      mxmlDelete(tree)
      tree:=NIL
      //SET DATE A_SET_DAT
      return a
    endif

    //set date format to "YYYY-MM-DD"
    tree:= mxmlNewXML()
    jpk := mxmlNewElement( tree, 'JPK' )

    mxmlElementSetAttr( jpk, "xmlns:etd", "http://crd.gov.pl/xml/schematy/dziedzinowe/mf/2016/01/25/eD/DefinicjeTypy/")
    mxmlElementSetAttr( jpk, HB_EOL()+chr(9)+"xmlns", D_MAGNAMESPACE)
    element   := mxmlNewElement( jpk, 'Naglowek' )

     node := mxmlNewElement( element, "KodFormularza")
      mxmlElementSetAttr( node, "kodSystemowy", "JPK_MAG ("+D_MAGVARIANT+")" )
      mxmlElementSetAttr( node, "wersjaSchemy", "1-0" )
      mxmlNewText( node,, "JPK_MAG")

     node := mxmlNewElement( element, "WariantFormularza")
      mxmlNewText( node,, D_MAGVARIANT)

     node := mxmlNewElement( element, "CelZlozenia")
      mxmlNewText( node,, "1")

     //d:=set(_SET_DATEFORMAT,'YYYY-MM-DD')

     node := mxmlNewElement( element, "DataWytworzeniaJPK")
      mxmlNewText( node,, stuff(HB_TSTOSTR(HB_TSTOUTC(hb_DateTime())),11,1,'T')+'Z')

     node := mxmlNewElement( element, "DataOd")
      mxmlNewText( node,, hb_dtoc(od,'YYYY-MM-DD') )

     node := mxmlNewElement( element, "DataDo")
      mxmlNewText( node,, hb_dtoc(do,'YYYY-MM-DD') )

      //set(_SET_DATEFORMAT,d)

     node := mxmlNewElement( element, "DomyslnyKodWaluty")
DEFAULT waluta TO 'PLN'
      mxmlNewText( node,, waluta )

     node := mxmlNewElement( element, "KodUrzedu")
      mxmlNewText( node,, memvar->firma_Urz )

    group   := mxmlNewElement( jpk, 'Podmiot1' )

     element := mxmlNewElement( group, 'IdentyfikatorPodmiotu' )

      node := mxmlNewElement( element, "etd:NIP")
       mxmlNewText( node,, trim(memvar->firma_NIP) )

      node := mxmlNewElement( element, "etd:PelnaNazwa")
       mxmlNewText( node,, trim(memvar->firma_PelNaz) )

      if !empty(memvar->firma_REGON)
      node := mxmlNewElement( element, "etd:REGON")
       mxmlNewText( node,, trim(memvar->firma_REGON) )
      endif

     element := mxmlNewElement( group, 'AdresPodmiotu' )

      node := mxmlNewElement( element, "etd:KodKraju")
       mxmlNewText( node,, "PL" )

      node := mxmlNewElement( element, "etd:Wojewodztwo")
       mxmlNewText( node,, trim(memvar->firma_Woj) )

      node := mxmlNewElement( element, "etd:Powiat")
       mxmlNewText( node,, trim(memvar->firma_Pow ))

      node := mxmlNewElement( element, "etd:Gmina")
       mxmlNewText( node,, trim(memvar->firma_Gmin ))

      if !empty(memvar->firma_ul)
      node := mxmlNewElement( element, "etd:Ulica")
       mxmlNewText( node,, trim(memvar->firma_Ul ))
      endif

      node := mxmlNewElement( element, "etd:NrDomu")
       mxmlNewText( node,, trim(memvar->firma_Dom ))

      node := mxmlNewElement( element, "etd:Miejscowosc")
       mxmlNewText( node,, trim(memvar->firma_miasto) )

      node := mxmlNewElement( element, "etd:KodPocztowy")
       mxmlNewText( node,, left(memvar->firma_poczta,6) )

      node := mxmlNewElement( element, "etd:Poczta")
       mxmlNewText( node,, trim(SubStr(memvar->firma_poczta,8)) )


    group   := mxmlNewElement( jpk, 'Magazyn' )
       mxmlNewText( group,, trim(magazyny[mag_poz])+trim(' '+adres_mag[mag_poz]) )

    group:=lp:=aj:=0

return jpk

func jpk_magPZ(da,ds,nd,do,wa,nrf)
local a,b,c,d,element,node,i

  if da=NIL
    if lp>0

    element   := mxmlNewElement( group, 'PZCtrl' )

     node := mxmlNewElement( element, 'LiczbaPZ' )
     mxmlNewText( node,,ltrim(tran(lp,)))

     node := mxmlNewElement( element, 'SumaPZ' )
     mxmlNewText( node,,ltrim(tran(aj/100,)))
    endif

     a:={lp,aj/100}

     group:=lp:=aj:=0

     return a

  endif

  if empty(lp)
    group := mxmlNewElement( jpk, 'PZ' )
    aj:=lp:=0
  endif

     //d:=set(_SET_DATEFORMAT,'YYYY-MM-DD')

    element   := mxmlNewElement( group, 'PZWartosc' )

     node := mxmlNewElement( element, 'NumerPZ' )
     mxmlNewText( node,,nd)

     node := mxmlNewElement( element, 'DataPZ' )
     mxmlNewText( node,,hb_dtoc(da,'YYYY-MM-DD') )

     node := mxmlNewElement( element, 'WartoscPZ' )
     mxmlNewText( node,,ltrim(tran(wa,)) )

     node := mxmlNewElement( element, 'DataOtrzymaniaPZ' )
     mxmlNewText( node,,hb_dtoc(da,'YYYY-MM-DD') )

     node := mxmlNewElement( element, 'Dostawca' )
     mxmlNewText( node,,trim(do) )

     if !empty(nrf)
       node := mxmlNewElement( element, 'NumerFaPZ' )
       mxmlNewText( node,,trim(nrf) )

      //if da<>ds
       node := mxmlNewElement( element, 'DataFaPZ' )
       mxmlNewText( node,,hb_dtoc(ds,'YYYY-MM-DD') )
      //endif

     endif

     //set(_SET_DATEFORMAT,d)

     aj+=round(100*wa,0)

return ++lp

func jpk_magPZWiersz(nd,kt,nt,il,jm,ce,wa)
local a,b,c,d,element,node,i


    element   := mxmlNewElement( group, 'PZWiersz' )

     node := mxmlNewElement( element, 'Numer2PZ' )
     mxmlNewText( node,,nd)

     node := mxmlNewElement( element, 'KodTowaruPZ' )
     mxmlNewText( node,,trim(kt))

     node := mxmlNewElement( element, 'NazwaTowaruPZ' )
     mxmlNewText( node,,trim(nt))

     node := mxmlNewElement( element, 'IloscPrzyjetaPZ' )
     mxmlNewText( node,,ltrim(tran(il,)))

     node := mxmlNewElement( element, 'JednostkaMiaryPZ' )
     mxmlNewText( node,,trim(jm))

     node := mxmlNewElement( element, 'CenaJednPZ' )
     mxmlNewText( node,,ltrim(tran(ce,)))

     node := mxmlNewElement( element, 'WartoscPozycjiPZ' )
     mxmlNewText( node,,ltrim(tran(wa,)))

return 0


func jpk_magWZ(da,ds,nd,do,wa,nrf)
local a,b,c,d,element,node,i

  if da=NIL
    if lp>0
    element   := mxmlNewElement( group, 'WZCtrl' )

     node := mxmlNewElement( element, 'LiczbaWZ' )
     mxmlNewText( node,,ltrim(tran(lp,)))

     node := mxmlNewElement( element, 'SumaWZ' )
     mxmlNewText( node,,ltrim(tran(aj/100,)))
    endif
     a:={lp,aj/100}

     group:=lp:=aj:=0

     return a

  endif

  if empty(lp)
    group := mxmlNewElement( jpk, 'WZ' )
    aj:=lp:=0
  endif

     //d:=set(_SET_DATEFORMAT,'YYYY-MM-DD')

    element   := mxmlNewElement( group, 'WZWartosc' )

     node := mxmlNewElement( element, 'NumerWZ' )
     mxmlNewText( node,,nd)

     node := mxmlNewElement( element, 'DataWZ' )
     mxmlNewText( node,,hb_dtoc(da,'YYYY-MM-DD') )

     node := mxmlNewElement( element, 'WartoscWZ' )
     mxmlNewText( node,,ltrim(tran(wa,)) )

     node := mxmlNewElement( element, 'DataWydaniaWZ' )
     mxmlNewText( node,,hb_dtoc(da,'YYYY-MM-DD') )

     node := mxmlNewElement( element, 'OdbiorcaWZ' )
     mxmlNewText( node,,trim(do) )

     if !empty(nrf)
       node := mxmlNewElement( element, 'NumerFaWZ' )
       mxmlNewText( node,,trim(nrf) )

      //if da<>ds
       node := mxmlNewElement( element, 'DataFaWZ' )
       mxmlNewText( node,,hb_dtoc(ds,'YYYY-MM-DD') )
      //endif

     endif

     //set(_SET_DATEFORMAT,d)

     aj+=round(100*wa,0)

return ++lp

func jpk_magWZWiersz(nd,kt,nt,il,jm,ce,wa)
local a,b,c,d,element,node,i


    element   := mxmlNewElement( group, 'WZWiersz' )

     node := mxmlNewElement( element, 'Numer2WZ' )
     mxmlNewText( node,,nd)

     node := mxmlNewElement( element, 'KodTowaruWZ' )
     mxmlNewText( node,,trim(kt))

     node := mxmlNewElement( element, 'NazwaTowaruWZ' )
     mxmlNewText( node,,trim(nt))

     node := mxmlNewElement( element, 'IloscWydanaWZ' )
     mxmlNewText( node,,ltrim(tran(il,)))

     node := mxmlNewElement( element, 'JednostkaMiaryWZ' )
     mxmlNewText( node,,trim(jm))

     node := mxmlNewElement( element, 'CenaJednWZ' )
     mxmlNewText( node,,ltrim(tran(ce,)))

     node := mxmlNewElement( element, 'WartoscPozycjiWZ' )
     mxmlNewText( node,,ltrim(tran(wa,)))

return 0

func jpk_magRW(da,nd,do,wa)
local a,b,c,d,element,node,i

  if da=NIL
    if lp>0
    element   := mxmlNewElement( group, 'RWCtrl' )

     node := mxmlNewElement( element, 'LiczbaRW' )
     mxmlNewText( node,,ltrim(tran(lp,)))

     node := mxmlNewElement( element, 'SumaRW' )
     mxmlNewText( node,,ltrim(tran(aj/100,)))
    endif
     a:={lp,aj/100}

     group:=lp:=aj:=0

     return a

  endif

  if empty(lp)
    group := mxmlNewElement( jpk, 'RW' )
    aj:=lp:=0
  endif

     //d:=set(_SET_DATEFORMAT,'YYYY-MM-DD')

    element   := mxmlNewElement( group, 'RWWartosc' )

     node := mxmlNewElement( element, 'NumerRW' )
     mxmlNewText( node,,nd)

     node := mxmlNewElement( element, 'DataRW' )
     mxmlNewText( node,,hb_dtoc(da,'YYYY-MM-DD') )

     node := mxmlNewElement( element, 'WartoscRW' )
     mxmlNewText( node,,ltrim(tran(wa,)) )

     node := mxmlNewElement( element, 'DataWydaniaRW' )
     mxmlNewText( node,,hb_dtoc(da,'YYYY-MM-DD') )

     node := mxmlNewElement( element, 'DokadRW' )
     mxmlNewText( node,,trim(do) )

     //set(_SET_DATEFORMAT,d)

     aj+=round(100*wa,0)

return ++lp

func jpk_magRWWiersz(nd,kt,nt,il,jm,ce,wa)
local a,b,c,d,element,node,i


    element   := mxmlNewElement( group, 'RWWiersz' )

     node := mxmlNewElement( element, 'Numer2RW' )
     mxmlNewText( node,,nd)

     node := mxmlNewElement( element, 'KodTowaruRW' )
     mxmlNewText( node,,trim(kt))

     node := mxmlNewElement( element, 'NazwaTowaruRW' )
     mxmlNewText( node,,trim(nt))

     node := mxmlNewElement( element, 'IloscWydanaRW' )
     mxmlNewText( node,,ltrim(tran(il,)))

     node := mxmlNewElement( element, 'JednostkaMiaryRW' )
     mxmlNewText( node,,trim(jm))

     node := mxmlNewElement( element, 'CenaJednRW' )
     mxmlNewText( node,,ltrim(tran(ce,)))

     node := mxmlNewElement( element, 'WartoscPozycjiRW' )
     mxmlNewText( node,,ltrim(tran(wa,)))

return 0

func jpk_magMM(da,nd,z,do,wa)
local a,b,c,d,element,node,i

  if da=NIL
    if lp>0
    element   := mxmlNewElement( group, 'MMCtrl' )

     node := mxmlNewElement( element, 'LiczbaMM' )
     mxmlNewText( node,,ltrim(tran(lp,)))

     node := mxmlNewElement( element, 'SumaMM' )
     mxmlNewText( node,,ltrim(tran(aj/100,)))
    endif
     a:={lp,aj/100}

     group:=lp:=aj:=0

     return a

  endif

  if empty(lp)
    group := mxmlNewElement( jpk, 'MM' )
    aj:=lp:=0
  endif

     //d:=set(_SET_DATEFORMAT,'YYYY-MM-DD')

    element   := mxmlNewElement( group, 'MMWartosc' )

     node := mxmlNewElement( element, 'NumerMM' )
     mxmlNewText( node,,nd)

     node := mxmlNewElement( element, 'DataMM' )
     mxmlNewText( node,,hb_dtoc(da,'YYYY-MM-DD') )

     node := mxmlNewElement( element, 'WartoscMM' )
     mxmlNewText( node,,ltrim(tran(wa,)) )

     node := mxmlNewElement( element, 'DataWydaniaMM' )
     mxmlNewText( node,,hb_dtoc(da,'YYYY-MM-DD') )
     
     if !empty(z)
       node := mxmlNewElement( element, 'SkadMM' )
       mxmlNewText( node,,trim(z) )
     endif

     if !empty(do)
       node := mxmlNewElement( element, 'DokadMM' )
       mxmlNewText( node,,trim(do) )
     endif

     //set(_SET_DATEFORMAT,d)

     aj+=round(100*wa,0)

return ++lp

func jpk_magMMWiersz(nd,kt,nt,il,jm,ce,wa)
local a,b,c,d,element,node,i


    element   := mxmlNewElement( group, 'MMWiersz' )

     node := mxmlNewElement( element, 'Numer2MM' )
     mxmlNewText( node,,nd)

     node := mxmlNewElement( element, 'KodTowaruMM' )
     mxmlNewText( node,,trim(kt))

     node := mxmlNewElement( element, 'NazwaTowaruMM' )
     mxmlNewText( node,,trim(nt))

     node := mxmlNewElement( element, 'IloscWydanaMM' )
     mxmlNewText( node,,ltrim(tran(il,)))

     node := mxmlNewElement( element, 'JednostkaMiaryMM' )
     mxmlNewText( node,,trim(jm))

     node := mxmlNewElement( element, 'CenaJednMM' )
     mxmlNewText( node,,ltrim(tran(ce,)))

     node := mxmlNewElement( element, 'WartoscPozycjiMM' )
     mxmlNewText( node,,ltrim(tran(wa,)))

return 0

stat FUNC wscb( node, where )
   static waslf := .f.
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

   ELSEIF where == MXML_WS_BEFORE_OPEN
      RETURN if(waslf,'',HB_EOL())+Replicate( chr(9), nLevel )

   ELSEIF where == MXML_WS_AFTER_CLOSE
      waslf:=.t.
      RETURN HB_EOL()

   elseif where == MXML_WS_BEFORE_CLOSE .and. waslf
      RETURN Replicate( chr(9), nLevel )

   ENDIF
   waslf:=.f.
   RETURN NIL /* Return NIL for no added whitespace... */
*******
#pragma BEGINDUMP

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

HB_FUNC( URLENCODE )
{
   const char * pszData = hb_parc( 1 );

   if( pszData )
   {
      HB_ISIZ nLen = hb_parclen( 1 );

      if( nLen )
      {
         HB_BOOL bComplete = hb_parldef( 2, HB_TRUE );
         HB_ISIZ nPos = 0, nPosRet = 0;

         /* Giving maximum final length possible */
         char * pszRet = ( char * ) hb_xgrab( nLen * 3 + 1 );

         while( nPos < nLen )
         {
            char cElem = pszData[ nPos ];

            if( cElem == ' ' )
            {
               pszRet[ nPosRet ] = '+';
            }
            else if( ( cElem >= 'A' && cElem <= 'Z' ) ||
                     ( cElem >= 'a' && cElem <= 'z' ) ||
                     ( cElem >= '0' && cElem <= '9' ) ||
                     cElem == '.' || cElem == ',' || cElem == '&' ||
                     cElem == ';' || cElem == '_' )
//                     cElem == '/' || cElem == ';' || cElem == '_' )

            {
               pszRet[ nPosRet ] = cElem;
            }
            else if( ! bComplete && ( cElem == ':' || cElem == '?' || cElem == '=' ) )
            {
               pszRet[ nPosRet ] = cElem;
            }
            else /* encode! */
            {
               HB_UINT uiVal;
               pszRet[ nPosRet++ ] = '%';
               uiVal = ( ( HB_UCHAR ) cElem ) >> 4;
               pszRet[ nPosRet++ ] = ( char ) ( ( uiVal < 10 ? '0' : 'A' - 10 ) + uiVal );
               uiVal = ( ( HB_UCHAR ) cElem ) & 0x0F;
               pszRet[ nPosRet ] = ( char ) ( ( uiVal < 10 ? '0' : 'A' - 10 ) + uiVal );
            }

            nPosRet++;
            nPos++;
         }

         hb_retclen_buffer( pszRet, nPosRet );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL,
                     HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
}
#pragma ENDDUMP
