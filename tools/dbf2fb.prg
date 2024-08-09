//hbmk2 -gtnul dbf2fb tfirebrd hbfbird.hbc ../common/cppl852m.c ../common/cp_utf8m.c 

#command DEFAULT <x> TO <y> => IF (<x>)=NIL;<x>:=<y>;ENDIF

//#require "hbfbird"

REQUEST DBFCDX

//#include "simpleio.ch"
//#include "hbgtinfo.ch"
#include "inkey.ch"
#include "set.ch"

STATIC oServer, lCreateTable := .F.

PROCEDURE Main( ... )

   LOCAL cTok
   LOCAL cHostName := "localhost:"
   LOCAL cUser := "SYSDBA"
   LOCAL cPassWord := "masterkey"
   LOCAL cDataBase, cTable, cFile, oTable, oRecord, i

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   SET DELETED ON

   REQUEST HB_LANG_PL
   HB_LANGSELECT('PL')
   REQUEST HB_CODEPAGE_UTF8MD
   REQUEST HB_CODEPAGE_PL852M
   HB_CDPSELECT('UTF8MD')

   rddSetDefault( "DBFNTX" )

   IF PCount() < 4
      help()
      QUIT
   ENDIF

   //hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )

   i := 1
   // Scan parameters and setup workings
   DO WHILE i <= PCount()

      cTok := hb_PValue( i++ )

      DO CASE
      CASE cTok == "-h"
         cHostName := hb_PValue( i++ )
         cDatabase := ':'

      CASE cTok == "-d"
         cDataBase := Lower(hb_PValue( i++ ))
         cDatabase := '/db/' + cDatabase + '.fdb'

      CASE cTok == "-t"
         cTable := Lower(hb_PValue( i++ ))

      CASE cTok == "-f"
         cFile := hb_PValue( i++ )

      CASE cTok == "-u"
         cUser := hb_PValue( i++ )

      CASE cTok == "-p"
         cPassWord := hb_PValue( i++ )

      CASE cTok == "-c"
         lCreateTable := .T.

      OTHERWISE
         help()
         QUIT
      ENDCASE
   ENDDO
   altd()
   cdbg()

   if lCreateTable .and. .not. hb_FileExists( cDatabase )
      FErase( cDatabase )
      // jak za krótkie indeksy to zwiększyć page size
      oServer := FBCreateDB( cHostName + cDatabase, cUser, cPassWord, 4096/*4096|8192|16384*/, 'UTF8', 3, 'UNICODE_CI_AI')
      IF oServer <> 1
         ? procline(),oServer
         QUIT
      ENDIF
   endif
   
   oServer := TFbServer():New( cHostName + cDatabase, cUser, cPassWord, 3 ) //, cDataBase )
   IF oServer:NetErr()
      ? procline(),oServer:ErrorNo(), oServer:Error()
      QUIT
   ENDIF

   if empty(cFile)
      oTable := oServer:Query('SELECT * FROM '+cTable+' FETCH FIRST 100 ROWS ONLY')
      IF oTable:NetErr()
         ? procline(),oTable:ErrorNo(), oTable:Error()
         QUIT
      ENDIF
      DO WHILE oTable:Fetch()
         oRecord := oTable:GetRow()
         ?
         FOR i := 1 TO oRecord:FCount()
            ?? oRecord:FieldGet( i ),', '
         NEXT
      ENDDO
      oRecord := NIL
      oTable := NIL


   elseif hb_FileExists(cFile)
      CHGDAT(cFile,cTable,lCreateTable)
   else
      i:=RAT(HB_PS(),cfile)
      SET DEFAULT TO (LEFT(cfile,i))
      if lower(right(cFile,4))<>'.dbf'
         cFile+="*.dbf"
      endif
      
      AEVAL(DIRECTORY(cfile),{|X|CHGDAT(X[1],,lCreateTable)})
   end if
   
   oServer:Destroy()

   RETURN

static function fielddef(fStruct)
   local f:='"'+trim(fStruct[1])+ '" ' 
   switch left(fStruct[2],1)
      case 'C'
         if fStruct[3]>36
            f+='VAR'
         endif
         f+='CHAR('+hb_ntos(fStruct[3])+')'
         exit
      case 'M'
         f+='BLOB SUB_TYPE 1'
         exit
      case 'D'
         f+='DATE'
         exit
      case 'T'
         f+='TIME'
         exit
      case '@'
         f+='TIMESTAMP'
         exit
      case 'B'
         f+='DOUBLE'
         exit
      case 'N'
         f+='NUMERIC('
         if fStruct[4]=0
            f+=hb_ntos(fStruct[3])+')'
         else
            f+=hb_ntos(fStruct[3]-1)+', '+hb_ntos(fStruct[4])+')'
         endif
         exit
      case 'L'
         f+='BOOLEAN'
         exit
      endswitch
return f

static procedure chgdat(cFile, cTable, lCreateTable)
   LOCAL oTable, oRecord, aDbfStruct, i, cQuery
   ? cFile
   IF File(strtran(cFile,subs(cFile,-3),'fpt'))
     i:='DBFCDX'
   ENDIF
   dbUseArea( .f., i, cFile, cTable,.F., .T. ,'PL852M')
   if indexord()<>0
      SET ORDER TO 0
      GO TOP
   end if
   
   aDbfStruct := dbStruct()

   if empty(cTable)
      cTable:= Alias()
   end if

   ?? '',cTable

   IF lCreateTable
      IF hb_AScan( oServer:ListTables(), cTable,,, .T. ) > 0
         oServer:DeleteTable( cTable )
         IF oServer:NetErr()
            ? procline(),oServer:ErrorNo(), oServer:Error()
            ?
            break
         ENDIF
      ENDIF

      cQuery := 'CREATE TABLE "'+cTable+'"'+hb_eol()+' ( '
      aeval(aDbfStruct,{|x|cQuery += fielddef(x) + ', '})

      cQuery := left(cQuery,len(cQuery)-2)+hb_eol()+')'
      ? cQuery
      oServer:Execute( cQuery )
      IF oServer:NetErr()
         ? procline(),oServer:ErrorNo(), oServer:Error()
         ?
         break
      ENDIF
      dbCloseArea()
      RETURN
   ENDIF
   ?

   begin sequence

      oTable := oServer:Query( 'SELECT "DELETED" FROM "' + cTable + '" FETCH FIRST 1 ROW ONLY' )
      set(_SET_DELETED,oTable:NetErr())
      GO TOP

      // Initialize MySQL table
      oTable := oServer:Query( 'SELECT * FROM "' + cTable + '" FETCH FIRST 1 ROW ONLY' )
      IF oTable:NetErr()
         ?
         ? procline(),oTable:ErrorNo(), oTable:Error()
         ? 
         break
      ENDIF

      oRecord := oTable:GetBlankRow()
      asize(oRecord:aRow ,if(set(_SET_DELETED),FCount(),FCount()+1))

      DO WHILE ! Eof() .AND. Inkey() != K_ESC

         if !set(_SET_DELETED)
            oRecord:FieldPut('deleted',deleted())
         endif

         FOR i := 1 TO FCount()
            oRecord:FieldPut( FieldName( i ), FieldGet( i ) )
         NEXT
   
         dbSkip()

         begin sequence
            oServer:Append( oRecord, .f. )
            IF oServer:NetErr()
               ?
               ? procline(),oServer:ErrorNo(),oServer:Error()
               ? 
               break
            ENDIF
         recover
         end
         
      ENDDO
   recover
   end
   dbCloseArea()
   oTable := NIL
   RETURN

PROCEDURE Help()

   ? "dbf2MySQL - dbf file to MySQL table conversion utility"
   ? "-h hostname (default: localhost)"
   ? "-u user (default: root)"
   ? "-p password (default no password)"
   ? "-d name of database to use"
   ? "-t name of table to add records to"
   ? "-c delete existing table and create a new one"
   ? "-f name of .dbf file to import"
   ? "all parameters but -h -u -p -c are mandatory"
   ? ""

   RETURN

   #pragma BEGINDUMP
   #include "signal.h"
   #include "hbapi.h"
   
   HB_FUNC ( CDBG )
   {
   //   raise(SIGTRAP);
   }
   #pragma ENDDUMP   
   