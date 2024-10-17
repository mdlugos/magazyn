//hbmk2 dbf2mysq tmysql tsqlbrw hbmysql.hbc ../common/cppl852m.c ../common/cp_utf8m.c 

#command DEFAULT <x> TO <y> => IF (<x>)=NIL;<x>:=<y>;ENDIF

#require "hbmysql"

REQUEST DBFCDX

#include "inkey.ch"
#include "hbgtinfo.ch"
#include "set.ch"

STATIC oServer, lCreateTable := .F.

PROCEDURE Main( ... )

   LOCAL cTok
   LOCAL cHostName := ""
   LOCAL cUser := ""
   LOCAL cPassWord := ""
   LOCAL cDataBase, cTable, cFile, oBrowser, oTable
   LOCAL i

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   SET DELETED ON

   REQUEST HB_LANG_PL
   HB_LANGSELECT('PL')
   REQUEST HB_CODEPAGE_UTF8EX
   REQUEST HB_CODEPAGE_PL852M
   HB_CDPSELECT('UTF8EX')

   rddSetDefault( "DBFNTX" )

   IF PCount() < 4
      help()
      QUIT
   ENDIF

   hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )

   i := 1
   // Scan parameters and setup workings
   DO WHILE i <= PCount()

      cTok := hb_PValue( i++ )

      DO CASE
      CASE cTok == "-h"
         cHostName := hb_PValue( i++ )

      CASE cTok == "-d"
         cDataBase := Lower(hb_PValue( i++ ))

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

   oServer := TMySQLServer():New( cHostName, cUser, cPassWord) //, cDataBase )
   IF oServer:NetErr()
      ? oServer:Error()
      QUIT
   ENDIF

   oServer:Query("SET CHARACTER SET utf8mb4")
   IF oServer:NetErr()
      ? oServer:Error()
      QUIT
   ENDIF

   //oServer:Query("SET sql_mode = 'PAD_CHAR_TO_FULL_LENGTH'")
   oServer:Query("CREATE DATABASE IF NOT EXISTS "+cDataBase+" COLLATE utf8mb4_0900_ai_ci")
   IF oServer:NetErr()
      ? oServer:Error()
      QUIT
   ENDIF

   oServer:SelectDB( cDataBase )
   IF oServer:NetErr()
      ? oServer:Error()
      QUIT
   ENDIF
   
   if empty(cFile)
      oTable := oServer:Query("SELECT * FROM `"+cTable+"`")
      oBrowser := TBrowseSQL():New( 1, 1, maxrow() -1 , maxcol() - 1, oServer, oTable, cTable )
      @ 0, 0, maxrow(), maxcol() box hb_UTF8ToStrBox("╒═╕│╛═╘│")
      @ 2, 0 say "╞"
      @ 2, maxcol() say "╡"
      oBrowser:headSep := hb_UTF8ToStrBox(" ═")
      oBrowser:BrowseTable(.t.)
      oBrowser := NIL
      //oTable:sql_Commit()
      oTable := NIL


   elseif lower(right(cFile,4))<>'.dbf'
      i:=RAT(HB_PS(),cfile)
      SET DEFAULT TO (LEFT(cfile,i))
      AEVAL(DIRECTORY(cfile+"*.dbf"),{|X|CHGDAT(X[1],,lCreateTable)})
   else
      CHGDAT(cFile,cTable,lCreateTable)
   end if
   
   oServer:Destroy()

   RETURN

static proc chgdat(cFile, cTable, lCreateTable)
   LOCAL oTable, oRecord, aDbfStruct, i

   IF File(strtran(cFile,subs(cFile,-3),'fpt'))
     i:='DBFCDX'
   ENDIF
   dbUseArea( .f., i, cFile,,.F., .T. ,'PL852M')
   if indexord()<>0
      SET ORDER TO 0
      GO TOP
   end if
   
   aDbfStruct := dbStruct()

   if empty(cTable)
      cTable:= lower(Alias())
   end if

   IF lCreateTable
      IF hb_AScan( oServer:ListTables(), cTable,,, .T. ) > 0
         oServer:DeleteTable( cTable )
         IF oServer:NetErr()
            ? oServer:Error()
            ?
            break
         ENDIF
      ENDIF
      aeval(aDbfStruct,{|x,y|x[2]:=left(x[2],1)})
      oServer:CreateTable( cTable, aDbfStruct )
      IF oServer:NetErr()
         ? oServer:Error()
         ?
         break
      ENDIF
      dbCloseArea()
      RETURN
   ENDIF

   begin sequence

      oTable := oServer:Query( "SELECT `deleted` FROM `" + cTable + "` LIMIT 1" )
      set(_SET_DELETED,oTable:NetErr())
      GO TOP

      // Initialize MySQL table
      oTable := oServer:Query( "SELECT * FROM `" + cTable + "` LIMIT 1" )
      IF oTable:NetErr()
         ? oTable:Error()
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
            oTable:Append( oRecord, .f. )
            IF oTable:NetErr()
               ? oTable:Error()
               ? 
               break
            ENDIF
         recover
         end
   
         DevPos( Row(), 1 )
         IF ( RecNo() % 100 ) == 0
            DevOut( "imported recs: " + hb_ntos( RecNo() ) )
         ENDIF
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
