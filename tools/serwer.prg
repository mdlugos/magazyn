//hbmk2 serwer -gtnul hbwin.hbc sddodbc.hbc
//"Server=SKLEP\INSERTNEXO;Driver={SQL Server};DSN=;Uid=sa;database={Nexo_PHU ZAGRODA A.Szlaur};pwd=srv1" "10.0.0.2" "1001" "Qoltec 50245"
#include "dbinfo.ch"
#require "rddsql"
#require "sddodbc"

#ifdef __PLATFORM__WINDOWS
  #require "hbwin"
#endif
#ifdef __PALTFORM__UNIX
  #require "hbcups"
#endif

#include "simpleio.ch"

#define D_LEN 30
//37

REQUEST SQLMIX, SDDODBC
REQUEST HB_LANG_PL
REQUEST HB_CODEPAGE_PL852

proc main(path, ip, port, lpt)
  local time, i := 0, oprn
  local s := space(36)
  local a,ad,k,n,t,socket,sock_out
  local ht := { => }
  LOCAL nConnection, nI, aI
  local sql //:= 'SELECT a.Symbol AS [Index], a.Nazwa ,c.CenaBrutto AS [Cena] FROM [ModelDanychContainer].[Asortymenty] AS a INNER JOIN [ModelDanychContainer].[PozycjeCennika] AS c ON c.Cennik_Id = 100000 and a.id = c.Asortyment_Id WHERE a.INDEX="'
  local LastId:=0
  
   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   HB_LANGSELECT('PL')
   HB_CDPSELECT('PL852')
   rddSetDefault( "SQLMIX" )

if !empty(path)  .and. type(path)='N'
  lastID:=val(path)
  path:=NIL
endif
if empty(path)
  path := "dsn=zagroda;uid=sa;pwd=srv1"
endif
   ?? 'ODBC Connecting to:',path
   ?
   nConnection := RDDINFO(RDDI_CONNECT,{"ODBC",path,"SQLMIX"})
   IF nConnection == 0
      ? "Unable connect to server", rddInfo( RDDI_ERRORNO ), rddInfo( RDDI_ERROR )
      ?
      RETURN 1
   ENDIF
   ?? 'OK'
   ?
  //set default to (path)

#ifdef __PLATFORM__WINDOWS
if empty(lpt)
   ? "Printer List:"
   aeval(win_printerList(),{|x|outstd(hb_eol()),outstd(x)})
   ?
   oprn:=.t.
else
   ? 'PRINT to:',lpt
   ?
   oprn:=Win_Prn():New( LPT )
   ?
endif
#endif

#ifdef __PLATFORM__UNIX
if empty(lpt)
  ? "Printer List:"
  FOR EACH i IN cupsGetDests()
    ? i:__enumIndex(), i
  NEXT
  ?
else
  ? 'PRINT to:',lpt 
  //cupsGetDefault()
  ?
endif
#endif

hb_inetInit()

if empty(port)
  port:=1001
else
  port:=val(port)
endif
if empty(ip)
   ip := '10.0.0.2'
endif

socket := hb_inetDGramBind(port, , ip)

if hb_inetErrorCode(socket) <> 0
  ?? hb_inetErrorCode(socket),hb_inetErrorDesc(socket)
  ?
  return 1
endif

hb_inetTimeout( socket, 500)

do while .t.

   l:=hb_inetDGramRecv(socket, @s)

   if l<=5
       hb_idlestate()
       ?? subs('-\|/',i:=i%4+1,1)+chr(8)
#ifdef __PLATFORM__WINDOWS
    if !empty(oprn)
     USE "SELECT TOP 1 Id FROM [ModelDanychContainer].[PozycjeDokumentu] ORDER BY Id DESC"
     if LastId<=0
        LastId+=Id //- 100
        loop
     endif
     if LastId<Id
       sql:="SELECT TOP 100 d.Wystawil, p.Dokument_Id, p.Id, p.Ilosc, a.Nazwa, j.Symbol FROM ";
        +"[ModelDanychContainer].[PozycjeDokumentu] AS p ";
        +"INNER JOIN [ModelDanychContainer].[CechyAsortymentuAsortyment] AS c ";
        +"ON p.AsortymentAktualnyId = c.Asortymenty_Id AND c.Cechy_Id=100049 ";
        +"INNER JOIN [ModelDanychContainer].[Dokumenty] AS d ";
        +"ON d.Id = p.Dokument_Id AND d.MiejsceDostawyTyp = 1 ";
        +"AND (d.Symbol = 'PA' OR d.Symbol = 'FS' )";
        +"LEFT JOIN [ModelDanychContainer].[Asortymenty] AS a ";
        +"ON c.Asortymenty_Id = a.Id ";
        +"LEFT JOIN [ModelDanychContainer].[JednostkiMiarAsortymentow] AS b ";
        +"ON c.Asortymenty_Id = b.Asortyment_Id ";
        +"LEFT JOIN [ModelDanychContainer].[JednostkiMiar] AS j ";
        +"ON b.JednostkaMiary_Id = j.Id ";
        +"WHERE p.Id BETWEEN "+ltrim(str(LastId+1))+" AND "+ltrim(str(Id));
       
       LastId:=Id
       USE (sql)
     else
       loop
     endif
     if !eof()
       if valtype(oprn)<>'L'
         if !oprn:Create()
            ? "Unable to connect to the printer"
            ?
            return 1
         endif
       endif
       do while !eof()
         if valtype(oprn)<>'L'
           oprn:StartDoc()
           //oPrn:SetFont( "Courier New", 7, { 3, -50 } )
           oPrn:SetFont( "Courier New", 11, { 3, -30 } )
           oprn:TextOut("",.t.)
         endif
         ? a:=padc(wystawil,D_LEN,"-")
         if valtype(oprn)<>'L'
           oprn:TextOut(a,.t.)
         endif
         ? a:="**Wydanie "+tran(date(),)+"  "+tran(time(),)
         ?
         if valtype(oprn)<>'L'
           oprn:TextOut(a,.t.)
           oprn:TextOut("",.t.)
         //oPrn:SetFont( "Courier New", 10 )
           oPrn:Bold( WIN_FW_SEMIBOLD )
         endif
         i:=Dokument_Id
         do while Dokument_ID = i
           ? a:=strtran(str(ilosc,7,3),".000","")+" "+Symbol+" "+Trim(Nazwa)
           if valtype(oprn)<>'L'
             do while len(a)>0
                oprn:TextOut(left(a,D_LEN),.t.)
                a:=subs(a,D_LEN+1)
                if len(a)=0
                   oprn:TextOut("",.t.)
                elseif len(a)<D_LEN
                   a:=padl(a,D_LEN)
                endif
             enddo
           endif
           skip
         enddo
         if valtype(oprn)<>'L'
           oPrn:Bold( WIN_FW_NORMAL )
           oprn:EndDoc()
         endif
       enddo
       if valtype(oprn)<>'L'
         oprn:Destroy()
       endif
     endif
     USE
     hb_idlestate()
   endif
#endif   
   loop
   endif

     t := subs(s,3,1)
     if t = chr(6)
        loop
     endif

     k:=subs(s,4,13)
     n := asc(subs(s,2,1))

     if s = chr(2) .and. t = '1' //.and. !empty(a:=hb_HGetDef(ht, trim(left(k,12)),.f.))
       hb_idlestate()
       sql:="SELECT TOP 1 a.Nazwa ,c.CenaBrutto FROM [ModelDanychContainer].[Asortymenty] AS a INNER JOIN [ModelDanychContainer].[PozycjeCennika] AS c ON c.Cennik_Id = 100000 and a.id = c.Asortyment_Id WHERE a.Symbol LIKE '"+trim(left(k,12))+"%'"
       use (sql)
       //dbUseArea( .f., ,sql, [as_cen] )
       //dbclosearea()
       a := ltrim(str(cenabrutto,10,2))
       a := left(nazwa ,39 - len(a))+ ' ' + a
       ? a
       hb_inetDGramSend(socket, hb_inetAddress(socket), port, chr(2) + chr(n) + t + crc16(k) + a + chr(13) + '0' + '15' + chr(3))
       use
     endif

     hb_idlestate()
enddo

#ifdef __PLATFORM__WINDOWS
// never here
if valtype(oprn)<>'L'
   oprn:Destroy()
endif
#endif
return 0

#pragma BEGINDUMP
#include "hbapi.h"

HB_FUNC_STATIC ( CRC16 )
{
const unsigned short TablicaCRC16[] = {
0x0000, 0xC0C1, 0xC181, 0x0140, 0xC301, 0x03C0, 0x0280, 0xC241,
0xC601, 0x06C0, 0x0780, 0xC741, 0x0500, 0xC5C1, 0xC481, 0x0440,
0xCC01, 0x0CC0, 0x0D80, 0xCD41, 0x0F00, 0xCFC1, 0xCE81, 0x0E40,
0x0A00, 0xCAC1, 0xCB81, 0x0B40, 0xC901, 0x09C0, 0x0880, 0xC841,
0xD801, 0x18C0, 0x1980, 0xD941, 0x1B00, 0xDBC1, 0xDA81, 0x1A40,
0x1E00, 0xDEC1, 0xDF81, 0x1F40, 0xDD01, 0x1DC0, 0x1C80, 0xDC41,
0x1400, 0xD4C1, 0xD581, 0x1540, 0xD701, 0x17C0, 0x1680, 0xD641,
0xD201, 0x12C0, 0x1380, 0xD341, 0x1100, 0xD1C1, 0xD081, 0x1040,
0xF001, 0x30C0, 0x3180, 0xF141, 0x3300, 0xF3C1, 0xF281, 0x3240,
0x3600, 0xF6C1, 0xF781, 0x3740, 0xF501, 0x35C0, 0x3480, 0xF441,
0x3C00, 0xFCC1, 0xFD81, 0x3D40, 0xFF01, 0x3FC0, 0x3E80, 0xFE41,
0xFA01, 0x3AC0, 0x3B80, 0xFB41, 0x3900, 0xF9C1, 0xF881, 0x3840,
0x2800, 0xE8C1, 0xE981, 0x2940, 0xEB01, 0x2BC0, 0x2A80, 0xEA41,
0xEE01, 0x2EC0, 0x2F80, 0xEF41, 0x2D00, 0xEDC1, 0xEC81, 0x2C40,
0xE401, 0x24C0, 0x2580, 0xE541, 0x2700, 0xE7C1, 0xE681, 0x2640,
0x2200, 0xE2C1, 0xE381, 0x2340, 0xE101, 0x21C0, 0x2080, 0xE041,
0xA001, 0x60C0, 0x6180, 0xA141, 0x6300, 0xA3C1, 0xA281, 0x6240,
0x6600, 0xA6C1, 0xA781, 0x6740, 0xA501, 0x65C0, 0x6480, 0xA441,
0x6C00, 0xACC1, 0xAD81, 0x6D40, 0xAF01, 0x6FC0, 0x6E80, 0xAE41,
0xAA01, 0x6AC0, 0x6B80, 0xAB41, 0x6900, 0xA9C1, 0xA881, 0x6840,
0x7800, 0xB8C1, 0xB981, 0x7940, 0xBB01, 0x7BC0, 0x7A80, 0xBA41,
0xBE01, 0x7EC0, 0x7F80, 0xBF41, 0x7D00, 0xBDC1, 0xBC81, 0x7C40,
0xB401, 0x74C0, 0x7580, 0xB541, 0x7700, 0xB7C1, 0xB681, 0x7640,
0x7200, 0xB2C1, 0xB381, 0x7340, 0xB101, 0x71C0, 0x7080, 0xB041,
0x5000, 0x90C1, 0x9181, 0x5140, 0x9301, 0x53C0, 0x5280, 0x9241,
0x9601, 0x56C0, 0x5780, 0x9741, 0x5500, 0x95C1, 0x9481, 0x5440,
0x9C01, 0x5CC0, 0x5D80, 0x9D41, 0x5F00, 0x9FC1, 0x9E81, 0x5E40,
0x5A00, 0x9AC1, 0x9B81, 0x5B40, 0x9901, 0x59C0, 0x5880, 0x9841,
0x8801, 0x48C0, 0x4980, 0x8941, 0x4B00, 0x8BC1, 0x8A81, 0x4A40,
0x4E00, 0x8EC1, 0x8F81, 0x4F40, 0x8D01, 0x4DC0, 0x4C80, 0x8C41,
0x4400, 0x84C1, 0x8581, 0x4540, 0x8701, 0x47C0, 0x4680, 0x8641,
0x8201, 0x42C0, 0x4380, 0x8341, 0x4100, 0x81C1, 0x8081, 0x4040};

   unsigned short l = hb_parclen(1), Licznik = 0, WartoscSumy = 0;
   const char* DaneDoSumy = hb_parc(1);
   char szResult[ 2 ];

   while (Licznik < l)
   {
     unsigned char WskaznikTablicy = (unsigned char)WartoscSumy ^ (unsigned char)DaneDoSumy[Licznik++];
     WartoscSumy = (WartoscSumy >> 8) ^ TablicaCRC16[WskaznikTablicy];
   }

   WartoscSumy |= 0x8080;

   HB_PUT_LE_UINT16( szResult, WartoscSumy );
   hb_retclen( szResult, 2 );
}
#pragma ENDDUMP
