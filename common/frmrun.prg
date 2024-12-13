/***
*   Frmrun.prg
*   Clipper 5.0 REPORT FORM runtime system
*   Copyright (c) 1990 Nantucket Corp.  All rights reserved.
*
*   Compile: /n/w/m
*/


#include "frmdef.ch"
#include "error.ch"

#ifdef A_XPRN
memvar p_coln
#else
//#define P_ROWN 58
#ifdef A_PCL
#define P_COLN 78
#else
#define P_COLN 80
#endif
#endif

// Definitions for buffer sizes
#define  SIZE_FILE_BUFF             1990       // Size of report file
#define  SIZE_LENGTHS_BUFF          110
#define  SIZE_OFFSETS_BUFF          110
#define  SIZE_EXPR_BUFF             1440
#define  SIZE_FIELDS_BUFF           300
#define  SIZE_PARAMS_BUFF           24

// Definitions for offsets into the FILE_BUFF string
#define  LENGTHS_OFFSET             5          // Start of expression length array
#define  OFFSETS_OFFSET             115        // Start of expression position array
#define  EXPR_OFFSET                225        // Start of expression data area
#define  FIELDS_OFFSET              1665       // Start of report columns (fields)
#define  PARAMS_OFFSET              1965       // Start of report parameters block

// These are offsets into the FIELDS_BUFF string to actual values
// Values are added to a block offset FLD_OFFSET that is moved in 
// increments of 12 
#define  FIELD_WIDTH_OFFSET         1
#define  FIELD_TOTALS_OFFSET        6
#define  FIELD_DECIMALS_OFFSET      7

// These are offsets into FIELDS_BUFF which are used to 'point' into
// the EXPR_BUFF string which contains the textual data 
#define  FIELD_CONTENT_EXPR_OFFSET  9
#define  FIELD_HEADER_EXPR_OFFSET   11

// These are actual offsets into the PARAMS_BUFF string which
// are used to 'point' into the EXPR_BUFF string 
#define  PAGE_HDR_OFFSET            1
#define  GRP_EXPR_OFFSET            3
#define  SUB_EXPR_OFFSET            5
#define  GRP_HDR_OFFSET             7
#define  SUB_HDR_OFFSET             9

// These are actual offsets into the PARAMS_BUFF string to actual values
#define  PAGE_WIDTH_OFFSET          11
#define  LNS_PER_PAGE_OFFSET        13
#define  LEFT_MRGN_OFFSET           15
#define  RIGHT_MGRN_OFFSET          17
#define  COL_COUNT_OFFSET           19
#define  DBL_SPACE_OFFSET           21
#define  SUMMARY_RPT_OFFSET         22
#define  PE_OFFSET                  23
#define  OPTION_OFFSET              24

// File error definitions
#define  F_OK                       0          // No error
#define  F_EMPTY                   -3          // File is empty
#define  F_ERROR                   -1          // Some kind of error
#define  F_NOEXIST                  2          // File does not exist

// Declare file-wide statics
STATIC cExprBuff
STATIC cOffsetsBuff
STATIC cLengthsBuff

STATIC aReportData, nPageNumber, nLinesLeft, aReportTotals
STATIC aGroupTotals, lFirstPass, lFormFeeds, nMaxLinesAvail

memvar firma_n

FUNCTION __ReportForm( cFRMName, lPrinter, cAltFile, lNoConsole, bFor, ;
                       bWhile, nNext, nRecord, lRest, lPlain, cHeading, ;
                       lBEject, lSummary )

   LOCAL lPrintOn, lConsoleOn // Status of PRINTER and CONSOLE
   LOCAL cExtraFile, lExtraState  // Status of EXTRA
   LOCAL nCol, nGroup
   LOCAL xBreakVal, lBroke := .F.
   LOCAL err

   LOCAL lAnyTotals
   LOCAL lAnySubTotals
   begin sequence

   // Resolve parameters
    IF AT( ".", cFRMName ) == 0
     cFRMName := TRIM( cFRMName ) + ".frm"
    ENDIF

   IF lPrinter == NIL
    lFormFeeds := SET( _SET_PRINTER )
   ELSE
    lPrintOn   := SET( _SET_PRINTER )
    lFormFeeds := lPrinter
    if lPrinter .and. !lPrintOn .and. !print(2)
       lPrinter   := .f.
    endif
   ENDIF

   IF cHeading == NIL
    cHeading := ""
   ENDIF

   IF lNoConsole#NIL 
    lConsoleOn := SET( _SET_CONSOLE, !lNoConsole  )
   ENDIF


   IF (!Empty(cAltFile))        // To file
    cExtraFile := SET( _SET_EXTRAFILE, cAltFile )
    lExtraState := SET( _SET_EXTRA, .T. )
   ENDIF


    aReportData := __FrmLoad( cFRMName )   // Load the frm into an array
    nMaxLinesAvail := aReportData[RP_LINES]

    // Modify aReportData based on the report parameters
    IF lSummary != NIL           // Set the summary only flag
     aReportData[ RP_SUMMARY ] := lSummary
    ENDIF

    aReportData[ RP_BEJECT ] := .F.        //==========================

    aReportData[ RP_AEJECT ] := .t.        //==========================

    IF lPlain#NIL .and. lPlain               // Set plain report flag
     aReportData[ RP_PLAIN ]   := .T.
     cHeading            := ""
     lFormFeeds          := .F.
    ENDIF
    aReportData[ RP_HEADING ]    := cHeading

    // Add to the left margin if a SET MARGIN has been defined
    // NOTE: uncommenting this line will cause REPORT FORM to respect
    // SET MARGIN to screen/to file, but double the margin TO PRINT
    // aReportData[ RP_LMARGIN ] += SET( _SET_MARGIN )

    nPageNumber := 1             // Set the initial page number
    lFirstPass  := .T.           // Set the first pass flag

    nLinesLeft  := aReportData[ RP_LINES ]

    // Check to see if a "before report" eject, or TO FILE has been specified
    IF aReportData[ RP_BEJECT ]
     EjectPage()
    ENDIF
    #command ? [ <list,...> ] => WOut( <list> )
    #command ?? [ <list,...> ] => WWOut( <list> )
    if lPrinter
#ifdef A_15CALI
    IF aReportData[ RP_WIDTH ] >96 .and. 1=alarm("CZY DRUKUJESZ NA SZEROKIM PAPIERZE",{"TAK","NIE"})
#ifdef A_17CPI
    IF aReportData[ RP_WIDTH ] <=136
#else
    IF aReportData[ RP_WIDTH ] >231
       ?? ccpi(8)
#endif
#ifdef A_15CPI
    elseIF aReportData[ RP_WIDTH ] >204
       ?? ccpi(7)
    elseIF aReportData[ RP_WIDTH ] >163
       ?? ccpi(6)
#else
    elseIF aReportData[ RP_WIDTH ] >163
       ?? ccpi(7)
#endif
    elseIF aReportData[ RP_WIDTH ] >136
       ?? ccpi(5)
    endif
    else
#endif
#ifdef A_17CPI
    IF aReportData[ RP_WIDTH ] <= P_COLN
#else
    IF aReportData[ RP_WIDTH ] > P_COLN * 1.66
       ?? ccpi(8)
#endif
#ifdef A_15CPI
    elseIF aReportData[ RP_WIDTH ] > P_COLN * 1.5
       ?? ccpi(7)
    elseIF aReportData[ RP_WIDTH ] > P_COLN * 1.2
       ?? ccpi(6)
#else
    elseIF aReportData[ RP_WIDTH ] > P_COLN * 1.2
       ?? ccpi(7)
#endif
    elseIF aReportData[ RP_WIDTH ] > P_COLN
       ?? ccpi(5)
    endif
#ifdef A_15CALI
    endif
#endif
    endif
    // Generate the initial report header manually (in case there are no
    // records that match the report scope)
    ReportHeader(@bfor,@bwhile)

    // Initialize aReportTotals to track both group and report totals, then
    // set the column total elements to 0 if they are to be totaled, otherwise
    // leave them NIL
    aReportTotals := ARRAY( LEN(aReportData[RP_GROUPS]) + 1, ;
                LEN(aReportData[RP_COLUMNS]) )

    // Column total elements
    FOR nCol := 1 TO LEN(aReportData[RP_COLUMNS])
     IF aReportData[RP_COLUMNS,nCol,RC_TOTAL]
      FOR nGroup := 1 TO LEN(aReportTotals)
         aReportTotals[nGroup,nCol] := 0
      NEXT
     ENDIF
    NEXT

    // Initialize aGroupTotals as an array
    aGroupTotals := ARRAY( LEN(aReportData[RP_GROUPS]) )

    // Execute the actual report based on matching records
    DBEval( { || ExecuteReport() }, bFor, bWhile, nNext, nRecord, lRest )

    // Generate any totals that may have been identified
    // Make a pass through all the groups
    FOR nGroup := LEN(aReportData[RP_GROUPS]) TO 1 STEP -1


     // make sure group has subtotals
     lAnySubTotals := .F.
     FOR nCol := 1 TO LEN(aReportData[RP_COLUMNS])
      IF aReportData[RP_COLUMNS,nCol,RC_TOTAL]
        lAnySubTotals := .T.
        EXIT          // NOTE
      ENDIF
     NEXT

     IF !lAnySubTotals
      LOOP            // NOTE
     ENDIF


     // Check to see if we need to eject the page
     IF nLinesLeft < 2
      EjectPage()
      IF aReportData[ RP_PLAIN ]
         nLinesLeft := 1000
      ELSE
         ReportHeader()
      ENDIF
     ENDIF

     // Print the first line
     PrintIt( SPACE(aReportData[RP_LMARGIN]) + ;
         IF(nGroup==1,"**   Grupa Razem       ****","*    Podgrupa Razem     ***") )

     // Print the second line

     ?? SPACE(aReportData[RP_LMARGIN])
     FOR nCol := 1 TO LEN(aReportData[RP_COLUMNS])
      scrltxt()
      IF nCol > 1
         ?? " "
      ENDIF
      IF aReportData[RP_COLUMNS,nCol,RC_TOTAL]
         ?? TRANSFORM(aReportTotals[nGroup+1,nCol], aReportData[RP_COLUMNS,nCol,RC_PICT])
      ELSE
         ?? SPACE(aReportData[RP_COLUMNS,nCol,RC_WIDTH])
      ENDIF
     NEXT

     // Send a cr/lf for the last line
     if row()>=MaxRow()-1
       if row()=MaxRow()
         scrllf()
         hb_scroll(0,0,MaxRow(),maxcol(),1)
       else
         setpos(MaxRow(),0)
       endif
       scrllf()
       ?
       setpos(MaxRow()-1,0)
       scrltxt()
     else
       ?
     endif

    NEXT

    // Generate the "Grand totals"
    // Check to see if we need to eject the page
    IF nLinesLeft < 2
     EjectPage()
     IF aReportData[ RP_PLAIN ]
      nLinesLeft := 1000
     ELSE
      ReportHeader()
     ENDIF
    ENDIF


    // Any report totals?
    lAnyTotals := .F.
    FOR nCol := 1 TO LEN(aReportData[RP_COLUMNS])
     IF aReportData[RP_COLUMNS,nCol,RC_TOTAL]
      lAnyTotals := .T.
      EXIT
     ENDIF
    NEXT


    IF lAnyTotals

      // Print the first line
      PrintIt( SPACE(aReportData[RP_LMARGIN]) + "*** Zestawienie Razem *****" )

      // Print the second line
      ?? SPACE(aReportData[RP_LMARGIN])
      FOR nCol := 1 TO LEN(aReportData[RP_COLUMNS])
       scrltxt()
       IF nCol > 1
        ?? " "
       ENDIF
       IF aReportData[RP_COLUMNS,nCol,RC_TOTAL]
        ?? TRANSFORM(aReportTotals[1,nCol], aReportData[RP_COLUMNS,nCol,RC_PICT])
       ELSE
        ?? SPACE(aReportData[RP_COLUMNS,nCol,RC_WIDTH])
       ENDIF
      NEXT

      // Send a cr/lf for the last line
     if row()>=MaxRow()-1
       if row()=MaxRow()
         scrllf()
         hb_scroll(0,0,MaxRow(),maxcol(),1)
       else
         setpos(MaxRow(),0)
       endif
       scrllf()
       ?
       scrltxt()
       setpos(MaxRow()-1,0)
     else
       ?
     endif

    END


    // Check to see if an "after report" eject, or TO FILE has been specified
    IF aReportData[ RP_AEJECT ]
     EjectPage()
    ENDIF


   END  // Sequence


   // Clean up and leave
   aReportData    := NIL         // Recover the space
   aReportTotals  := NIL
   aGroupTotals   := NIL
   nPageNumber    := NIL
   lFirstPass    := NIL
   nLinesLeft    := NIL
   lFormFeeds    := NIL
   nMaxLinesAvail := NIL

   // clean up
   // SET( _SET_PRINTER, lPrintOn )     // Set the printer back to prior state
   // SET( _SET_CONSOLE, lConsoleOn )     // Set the console back to prior state

   IF (!Empty(cAltFile))         // Set extrafile back
    SET( _SET_EXTRAFILE, cExtraFile )
    SET( _SET_EXTRA, lExtraState )
   ENDIF

   RETURN NIL



/***
*    ExecuteReport() --> NIL
*    Executed by DBEVAL() for each record that matches record scope
*/
STATIC FUNCTION ExecuteReport()
   LOCAL aRecordHeader  := {}           // Header for the current record
   LOCAL aRecordToPrint := {}           // Current record to print
   LOCAL nCol                           // Counter for the column work
   LOCAL nGroup                         // Counter for the group work
   LOCAL nGroupChanged                  // Has any group changed?
   LOCAL nMaxLines                      // Number of lines needed by record
   LOCAL nLine                          // Counter for each record line
   LOCAL cLine                          // Current line of text for parsing
   LOCAL nLastElement                   // Last element pointer if record is
                                        // too large for a page
   LOCAL lAnySubTotals
   local spacebuf

   // Add to the main column totals
   FOR nCol := 1 TO LEN(aReportData[RP_COLUMNS])
      IF aReportData[RP_COLUMNS,nCol,RC_TOTAL]
         // If this column should be totaled, do it
         aReportTotals[ 1 ,nCol] += ;
                  EVAL( aReportData[RP_COLUMNS,nCol,RC_EXP] )
      ENDIF
   NEXT

   // Determine if any of the groups have changed.  If so, add the appropriate
   // line to aRecordHeader for totaling out the previous records
   IF !lFirstPass                       // Don't bother first time through

      // Make a pass through all the groups
      nGroupChanged  := LEN(aReportData[RP_GROUPS])+1
      FOR nGroup := 1 TO LEN(aReportData[RP_GROUPS])
     // make sure group has subtotals
     lAnySubTotals := .F.
     FOR nCol := 1 TO LEN(aReportData[RP_COLUMNS])
      IF aReportData[RP_COLUMNS,nCol,RC_TOTAL]
         lAnySubTotals := .T.
         EXIT           // NOTE
      ENDIF
     NEXT

     IF !lAnySubTotals
      LOOP            // NOTE
     ENDIF
     IF MakeAStr(EVAL(aReportData[RP_GROUPS,nGroup,RG_EXP]),;
        aReportData[RP_GROUPS,nGroup,RG_TYPE]) != aGroupTotals[nGroup]

        nGroupChanged := nGroup
        exit
     ENDIF

      NEXT

         // If this group has changed since the last record
      FOR nGroup := LEN(aReportData[RP_GROUPS]) TO nGroupChanged STEP -1
            AADD( aRecordHeader, IF(nGroup==1,"**   Grupa Razem       ****","*    Podgrupa Razem     ***") )

            lAnySubTotals := .F.
            spacebuf:=0

            // Cycle through the columns, adding either the group
            // amount from aReportTotals or spaces wide enough for
            // the non-totaled columns
            FOR nCol := 1 TO LEN(aReportData[RP_COLUMNS])
               IF aReportData[RP_COLUMNS,nCol,RC_TOTAL]
                  if !lAnySubTotals
                     if spacebuf<27
                        AADD( aRecordHeader, "")
                     else
                        spacebuf-=27
                     endif
                  endif
                  lAnySubTotals := .t.
                  aRecordHeader[ LEN(aRecordHeader) ] += space(spacebuf) + ;
                     TRANSFORM(aReportTotals[nGroup+1,nCol], ;
                     aReportData[RP_COLUMNS,nCol,RC_PICT]) + " "
                  // Zero out the group totals column from aReportTotals
                  aReportTotals[nGroup+1,nCol] := 0
                  spacebuf:=0
               ELSE
                  spacebuf+=aReportData[RP_COLUMNS,nCol,RC_WIDTH]+1
               ENDIF
            NEXT
      NEXT
   else
      nGroupChanged:=1
      lFirstPass = .F.
   ENDIF


   // Add to aRecordHeader in the event that the group has changed and
   // new group headers need to be generated

   // Cycle through the groups
   if nGroupChanged <= LEN(aReportData[RP_GROUPS])
      AADD( aRecordHeader, "" )   // The blank line
   FOR nGroup := nGroupChanged TO LEN(aReportData[RP_GROUPS])
         AADD( aRecordHeader, IF(nGroup==1,"** ","* ") +;
               aReportData[RP_GROUPS,nGroup,RG_HEADER] + " " +;
               MakeAStr(EVAL(aReportData[RP_GROUPS,nGroup,RG_EXP]), ;
               aReportData[RP_GROUPS,nGroup,RG_TYPE]) )
   NEXT
   endif

   // Is there anything in the record header?
   IF LEN( aRecordHeader ) > 0
      // Determine if aRecordHeader will fit on the current page.  If not,
      // start a new header
      IF LEN( aRecordHeader ) > nLinesLeft
         EjectPage()
         IF aReportData[ RP_PLAIN ]
            nLinesLeft := 1000
         ELSE
            ReportHeader()
         ENDIF
      ENDIF

      // Send aRecordHeader to the output device, resetting nLinesLeft
      AEVAL( aRecordHeader, ;
          { | HeaderLine | ;
              PrintIt( SPACE(aReportData[RP_LMARGIN])+ HeaderLine ) ;
          } ;
      )
      nLinesLeft -= LEN( aRecordHeader )

      // Make sure it didn't hit the bottom margin
      IF nLinesLeft == 0
         EjectPage()
         IF aReportData[ RP_PLAIN ]
            nLinesLeft := 1000
         ELSE
            ReportHeader()
         ENDIF
      ENDIF
   ENDIF

   // Add to the group totals
   FOR nCol := 1 TO LEN(aReportData[RP_COLUMNS])
      // If this column should be totaled, do it
      IF aReportData[RP_COLUMNS,nCol,RC_TOTAL]
         // Cycle through the groups
         FOR nGroup := 1 TO LEN( aReportTotals ) - 1
            aReportTotals[nGroup+1,nCol] += ;
               EVAL( aReportData[RP_COLUMNS,nCol,RC_EXP] )
         NEXT
      ENDIF
   NEXT

   // Reset the group expressions in aGroupTotals
   FOR nGroup := 1 TO LEN(aReportData[RP_GROUPS])
      aGroupTotals[nGroup] := MakeAStr(EVAL(aReportData[RP_GROUPS,nGroup,RG_EXP]),;
                                    aReportData[RP_GROUPS,nGroup,RG_TYPE])
   NEXT

   // Only run through the record detail if this is NOT a summary report
   IF !aReportData[ RP_SUMMARY ]
      // Determine the max number of lines needed by each expression
    nMaxLines := 1  // $BH 2/14/91
      FOR nCol := 1 TO LEN(aReportData[RP_COLUMNS])
         IF aReportData[RP_COLUMNS,nCol,RC_TYPE] $ "CM"
            nMaxLines := MAX(XMLCOUNT(EVAL(aReportData[RP_COLUMNS,nCol,RC_EXP]),;
                         aReportData[RP_COLUMNS,nCol,RC_WIDTH]), nMaxLines)
         ENDIF
      NEXT

      // Size aRecordToPrint to the maximum number of lines it will need, then
      // fill it with nulls
      ASIZE( aRecordToPrint, nMaxLines )
      AFILL( aRecordToPrint, "" )

      // Load the current record into aRecordToPrint
      FOR nCol := 1 TO LEN(aReportData[RP_COLUMNS])
         FOR nLine := 1 TO nMaxLines
            // Check to see if it's a memo or character
            IF aReportData[RP_COLUMNS,nCol,RC_TYPE] $ "CM"
               // Load the current line of the current column into cLine
               cLine := XMEMOLINE(TRIM(EVAL(aReportData[RP_COLUMNS,nCol,RC_EXP])),;
                             aReportData[RP_COLUMNS,nCol,RC_WIDTH], nLine )
               cLine := PADR( cLine, aReportData[RP_COLUMNS,nCol,RC_WIDTH] )
            ELSE
               IF nLine == 1
                  cLine := TRANSFORM(EVAL(aReportData[RP_COLUMNS,nCol,RC_EXP]),;
                           aReportData[RP_COLUMNS,nCol,RC_PICT])
                  cLine := PADR( cLine, aReportData[RP_COLUMNS,nCol,RC_WIDTH] )
               ELSE
                  cLine := SPACE( aReportData[RP_COLUMNS,nCol,RC_WIDTH])
               ENDIF
            ENDIF
            // Add it to the existing report line
            IF nCol > 1
               aRecordToPrint[ nLine ] += " "
            ENDIF
            aRecordToPrint[ nLine ] += cLine
         NEXT
      NEXT

      // Determine if aRecordToPrint will fit on the current page
      IF LEN( aRecordToPrint ) > nLinesLeft
         // The record will not fit on the current page - will it fit on
         // a full page?  If not, break it up and print it.
         IF LEN( aRecordToPrint ) > nMaxLinesAvail
            // This record is HUGE!  Break it up...
            nLine := 1
            DO WHILE nLine < LEN( aRecordToPrint )
               PrintIt( SPACE(aReportData[RP_LMARGIN]) + aRecordToPrint[nLine] )
               nLine++
               nLinesLeft--
               IF nLinesLeft == 0
                  EjectPage()
                  IF aReportData[ RP_PLAIN ]
                     nLinesLeft := 1000
                  ELSE
                     ReportHeader()
                  ENDIF
               ENDIF
            ENDDO
         ELSE
            EjectPage()
            IF aReportData[ RP_PLAIN ]
               nLinesLeft := 1000
            ELSE
               ReportHeader()
            ENDIF
            AEVAL( aRecordToPrint, ;
               { | RecordLine | ;
                 PrintIt( SPACE(aReportData[RP_LMARGIN])+ RecordLine ) ;
               } ;
            )
            nLinesLeft -= LEN( aRecordToPrint )
         ENDIF
      ELSE
         // Send aRecordToPrint to the output device, resetting nLinesLeft
         AEVAL( aRecordToPrint, ;
            { | RecordLine | ;
              PrintIt( SPACE(aReportData[RP_LMARGIN])+ RecordLine ) ;
            } ;
         )
         nLinesLeft -= LEN( aRecordToPrint )
      ENDIF

      // Make sure it didn't hit the bottom margin
      IF nLinesLeft == 0
         EjectPage()
         IF aReportData[ RP_PLAIN ]
            nLinesLeft := 1000
         ELSE
            ReportHeader()
         ENDIF
      ENDIF

      // Tack on the spacing for double/triple/etc.
      IF aReportData[ RP_SPACING ] > 1
         IF nLinesLeft > aReportData[ RP_SPACING ] - 1
            FOR nLine := 2 TO aReportData[ RP_SPACING ]
               PrintIt()
               nLinesLeft--
            NEXT
         ENDIF
      ENDIF

   ENDIF    // Was this a summary report?

   RETURN NIL


STATIC FUNCTION ReportHeader(bfor,bwhile)
   LOCAL nLinesInHeader := 0
   LOCAL aPageHeader    := {}
   LOCAL nHeadingLength := aReportData[RP_WIDTH] - aReportData[RP_LMARGIN] - 30
   LOCAL nCol, nLine, nMaxColLength, nGroup, cHeader

   // Create the header and drop it into aPageHeader

   // Start with the heading
   AADD( aPageHeader, padr(firma_n, nHeadingLength+14)+" dnia "+dtoc(DatE()))
   AADD( aPageHeader, "" )
   IF !aReportData[ RP_PLAIN ]           // If not a plain paper report, build
      IF aReportData[RP_HEADING] == ""   // the heading
         AADD( aPageHeader, "Strona nr" + STR(nPageNumber,5) )
      ELSE
         nLinesInHeader := XMLCOUNT( aReportData[RP_HEADING], nHeadingLength )
         FOR nLine := 1 TO nLinesInHeader
            AADD( aPageHeader, SPACE(15) + ;
            PADC(TRIM(XMEMOLINE(aReportData[RP_HEADING],nHeadingLength,nLine)),;
                nHeadingLength))
         NEXT
         aPageHeader[ 3 ] := STUFF( aPageHeader[ 3 ], 1, 14, ;
                                    "Strona nr" + STR(nPageNumber,5) )
      ENDIF
      AADD( aPageHeader, "" )
   ENDIF


   // Tack on the actual header from the FRM
   if len(areportdata[RP_HEADER]) > 0
   FOR nLine := 1 TO LEN( aReportData[RP_HEADER] )
    cHeader:=precomp(aReportData[RP_HEADER,nLine],@bfor,@bwhile)
    if ""#cHeader
       AADD( aPageHeader, ;
             SPACE( ( aReportData[RP_WIDTH] - aReportData[RP_LMARGIN] - ;
             aReportData[RP_RMARGIN] - ;
             Len(cHeader) ) / 2 ) + cHeader )
    endif
   NEXT
      AADD( aPageHeader, "" )
   endif

   // Now add the column headings
   nLinesInHeader := LEN( aPageHeader )

   // Determine the longest column header
   nMaxColLength := 0
   FOR nCol := 1 TO LEN( aReportData[ RP_COLUMNS ] )
       nMaxColLength := MAX( LEN(aReportData[RP_COLUMNS,nCol,RC_HEADER]), ;
                             nMaxColLength )
   NEXT
   FOR nCol := 1 TO LEN( aReportData[ RP_COLUMNS ] )
      ASIZE( aReportData[RP_COLUMNS,nCol,RC_HEADER], nMaxColLength )
   NEXT

   if nmaxcollength>0

   FOR nLine := 1 TO nMaxColLength
      AADD( aPageHeader, "" )
   NEXT

   FOR nCol := 1 TO LEN(aReportData[RP_COLUMNS])    // Cycle through the columns
      FOR nLine := 1 TO nMaxColLength
         IF nCol > 1
            aPageHeader[ nLinesInHeader + nLine ] += " "
         ENDIF
         IF aReportData[RP_COLUMNS,nCol,RC_HEADER,nLine] == NIL
            aPageHeader[ nLinesInHeader + nLine ] += ;
                           SPACE( aReportData[RP_COLUMNS,nCol,RC_WIDTH] )
         ELSE
            IF aReportData[RP_COLUMNS,nCol,RC_TYPE] == "N"
               aPageHeader[ nLinesInHeader + nLine ] += ;
                           PADL(aReportData[RP_COLUMNS,nCol,RC_HEADER,nLine],;
                           aReportData[RP_COLUMNS,nCol,RC_WIDTH])
            ELSE
               aPageHeader[ nLinesInHeader + nLine ] += ;
                           PADR(aReportData[RP_COLUMNS,nCol,RC_HEADER,nLine],;
                           aReportData[RP_COLUMNS,nCol,RC_WIDTH])
            ENDIF
         ENDIF
      NEXT
   NEXT

   // Insert blank line between the heading and the actual data
   AADD( aPageHeader, "" )

   endif

   // Send it to the output device
//   IF lFirstPass
//      QOUT()
//   ENDIF

   AEVAL( aPageHeader, ;
      { | HeaderLine | ;
         PrintIt( SPACE(aReportData[RP_LMARGIN])+ HeaderLine ) ;
      } ;
   )

   // Set the page number and number of available lines
   nPageNumber++
   nLinesLeft := aReportData[RP_LINES] - LEN( aPageHeader )
   nMaxLinesAvail := aReportData[RP_LINES] - LEN( aPageHeader )

   RETURN NIL

/***
*    MakeStr( <exp>, <cType> ) --> value
*     Convert a value of any data type into string to add to the group header
*/
STATIC FUNCTION MakeAStr( uVar, cType )
   LOCAL cString
   DO CASE
   CASE UPPER(cType) == "D"
      cString := DTOC( uVar )

   CASE UPPER(cType) == "L"
      cString := IF( uVar, "T", "F" )

   CASE UPPER(cType) == "N"
      cString := STR( uVar )

   CASE UPPER(cType) == "C"
      cString := uVar

   OTHERWISE
      cString := "INVALID EXPRESSION"
   ENDCASE
   RETURN( cString )

/***
*    PrintIt( <cString> ) --> NIL
*    Print a string, THEN send a CRLF
*/
STATIC FUNCTION PrintIt( cString )

   IF cString == NIL
      cString := ""
   ELSE
    // prevents output of trailing space, also prevents wrapping of some
    // lines when sent to screen or 80-column printer. Comment out this
    // line for complete Summer 87 compatibility.
    cString := Trim( cString )
   ENDIF
   scrltxt()
   ?? cString

     if row()>=MaxRow()-1
       if row()=MaxRow()
         scrllf()
         hb_scroll(0,0,MaxRow(),maxcol(),1)
       else
         setpos(MaxRow(),0)
       endif
       scrllf()
       ?
       setpos(MaxRow()-1,0)
       scrltxt()
     else
       ?
     endif

   RETURN NIL

/***
*    EjectPage() --> NIL
*     Eject a page if the form feed option is set
*/
STATIC FUNCTION EjectPage
   IF lFormFeeds
      specout(chr(12))
   ENDIF
   RETURN NIL


STATIC FUNCTION XMLCOUNT( cString, nLineLength, nTabSize, lWrap )

   // Set defaults if none specified
   nLineLength := IF( nLineLength == NIL, 79, nLineLength )
   nTabSize := IF( nTabSize == NIL, 4, nTabSize )
   lWrap := IF( lWrap == NIL, .T., .F. )

   IF nTabSize >= nLineLength
      nTabSize := nLineLength - 1
   ENDIF

   RETURN( MLCOUNT( TRIM(cString), nLineLength, nTabSize, lWrap ) )


STATIC FUNCTION XMEMOLINE( cString, nLineLength, nLineNumber, nTabSize, lWrap )

   local ret

   // Set defaults if none specified
   nLineLength := IF( nLineLength == NIL, 79, nLineLength )
   nLineNumber := IF( nLineNumber == NIL, 1, nLineNumber )
   nTabSize := IF( nTabSize == NIL, 4, nTabSize )
   lWrap := IF( lWrap == NIL, .T., lWrap )

   IF nTabSize >= nLineLength
      nTabSize := nLineLength - 1
   ENDIF

return(MEMOLINE( cString, nLineLength, nLineNumber, nTabSize, lWrap ))

static func precomp(ret,bfor,bwhile)

   if ret='&:'
      ret:=&(trim(SubStr(ret,3)))
      if valtype(ret)="B"
         ret:=eval(ret)
      endif
   ELSEIF ret="FOR:"
      bfor:=&(trim(SubStr(ret,5)))
      ret:=""
   elseif ret="WHILE:"
      bwhile:=&(trim(SubStr(ret,7)))
      ret:=""
   endif

return(ret)

/***
*
*  __FrmLoad( cFrmFile ) --> aReport
*  Reads a report (.frm) file and creates a report array
*
*  Notes:
*
*      1.   Report file name has extension.
*      2.   File error number placed in nFileError
*      3.   Offsets start at 1. Offsets are into a Clipper string, 1 to 1990
*      4.   The offsets mentioned in these notes are actual DOS FILE offsets,
*           not like the offsets declared in the body of FrmLoad()
*           which are Clipper STRING offsets.
*      5.   Report file length is 7C6h (1990d) bytes.
*      6.   Expression length array starts at 04h (4d) and can
*           contain upto 55 short (2 byte) numbers.
*      7.   Expression offset index array starts at 72h (114d) and
*           can contain upto 55 short (2 byte) numbers.
*      8.   Expression area starts at offset E0h (224d).
*      9.   Expression area length is 5A0h (1440d).
*     10.   Expressions in expression area are null terminated.
*     11.   Field expression area starts at offset 680h (1664d).
*     12.   Field expressions (column definition) are null terminated.
*     13.   Field expression area can contain upto 25 12-byte blocks.
*/

FUNCTION __FrmLoad( cFrmFile )
  LOCAL cFieldsBuff
  LOCAL cParamsBuff
  LOCAL nFieldOffset := 0
  LOCAL cFileBuff    := SPACE(SIZE_FILE_BUFF)
  LOCAL cGroupExp    := SPACE(200)
  LOCAL cSubGroupExp := SPACE(200)
  LOCAL nColCount    := 0         // Number of columns in report
  LOCAL nCount
  LOCAL nFrmHandle              // (.frm) file handle
  LOCAL nPointer     := 0           // Points to an offset into EXPR_BUFF string
  LOCAL nFileError              // Contains current file error
  LOCAL cOptionByte              // Contains option byte

  LOCAL aReport[ RP_COUNT ]        // Create report array
   LOCAL err

  LOCAL s, paths
  LOCAL i

  // Initialize STATIC buffer values
  cLengthsBuff  := ""
  cOffsetsBuff  := ""
  cExprBuff     := ""

  // Default report values
  aReport[ RP_HEADER ]    := {}             
  aReport[ RP_WIDTH ]     := 80
  aReport[ RP_LMARGIN ]   := 8
  aReport[ RP_RMARGIN ]   := 0
  aReport[ RP_LINES ]     := 58
  aReport[ RP_SPACING ]   := 1              
  aReport[ RP_BEJECT ]    := .T.
  aReport[ RP_AEJECT ]    := .F.
  aReport[ RP_PLAIN ]     := .F.
  aReport[ RP_SUMMARY ]   := .F.
  aReport[ RP_COLUMNS ]   := {}
  aReport[ RP_GROUPS ]    := {}
  aReport[ RP_HEADING ]   := ""             

  // Open the report file

  nFrmHandle = FOPEN( cFrmFile )
  nFileError = FERROR()

  IF !( HB_ps() $ cFrmFile .or. ":" $ cFrmFile )
    // if not found and no path in name, go looking

    IF nFileError != F_OK

      s := SET( _SET_DEFAULT )

      IF !Empty( s )
        nFrmHandle := FOPEN( s + HB_ps() + cFrmFile )
        nFileError := FERROR()
      END
    END

    IF nFileError != F_OK

      s := SET( _SET_PATH )
      s := StrTran(s, ",", ";")   // convert any commas in path spec

      paths := ListAsArray( s )

      FOR i := 1 to Len(paths)
        nFrmHandle := FOPEN( paths[i] + HB_ps() + cFrmFile )
        nFileError := FERROR()

        IF nFileError == F_OK
          EXIT
        END
      NEXT
    END
  END

  // File error
  IF nFileError != F_OK
      err := ErrorNew()
      err:severity := 2
      err:genCode := EG_OPEN
      err:subSystem := "FRMLBL"
      Eval(ErrorBlock(), err)
  ENDIF

  // OPEN ok?
  IF nFileError = F_OK

        // Go to START of report file
     FSEEK(nFrmHandle, 0)

        // SEEK ok?
     nFileError = FERROR()
     IF nFileError = F_OK

        // Read entire file into process buffer
        nCount = fread(nFrmHandle, @cFileBuff , SIZE_FILE_BUFF)

        // READ ok?
        IF nCount = 0
           nFileError = F_EMPTY        // file is empty
        ELSE
           nFileError = FERROR()       // check for DOS errors
        ENDIF

        IF nFileError = F_OK

           // Is this a .FRM type file (2 at start and end of file)
          IF BIN2W(SUBSTR(cFileBuff, 1, 2)) = 2 .AND.;
              BIN2W(SUBSTR(cFileBuff, SIZE_FILE_BUFF - 1, 2)) = 2

              nFileError = F_OK
           ELSE
          nFileError = F_ERROR
      ENDIF

    ENDIF

  ENDIF

   // Close file
   IF !FCLOSE(nFrmHandle)
      nFileError = FERROR()
   ENDIF

ENDIF

// File existed, was opened and read ok and is a .FRM file
IF nFileError = F_OK

   // Fill processing buffers
   cLengthsBuff = SUBSTR(cFileBuff, LENGTHS_OFFSET, SIZE_LENGTHS_BUFF)
   cOffsetsBuff = SUBSTR(cFileBuff, OFFSETS_OFFSET, SIZE_OFFSETS_BUFF)
   cExprBuff    = SUBSTR(cFileBuff, EXPR_OFFSET, SIZE_EXPR_BUFF)
   cFieldsBuff  = SUBSTR(cFileBuff, FIELDS_OFFSET, SIZE_FIELDS_BUFF)
   cParamsBuff  = SUBSTR(cFileBuff, PARAMS_OFFSET, SIZE_PARAMS_BUFF)


   // Process report attributes
   // Report width
   aReport[ RP_WIDTH ]   := BIN2W(SUBSTR(cParamsBuff, PAGE_WIDTH_OFFSET, 2))

   // Lines per page
   aReport[ RP_LINES ]   := BIN2W(SUBSTR(cParamsBuff, LNS_PER_PAGE_OFFSET, 2))

   // Page offset (left margin)
   aReport[ RP_LMARGIN ] := BIN2W(SUBSTR(cParamsBuff, LEFT_MRGN_OFFSET, 2))

   // Page right margin (not used)
   aReport[ RP_RMARGIN ] := BIN2W(SUBSTR(cParamsBuff, RIGHT_MGRN_OFFSET, 2))

   nColCount  = BIN2W(SUBSTR(cParamsBuff, COL_COUNT_OFFSET, 2))

   // Line spacing
   // Spacing is 1, 2, or 3
   aReport[ RP_SPACING ] := IF(SUBSTR(cParamsBuff, DBL_SPACE_OFFSET, 1) $ "Yy", 2, 1)

   // Summary report flag
   aReport[ RP_SUMMARY ] := IF(SUBSTR(cParamsBuff, SUMMARY_RPT_OFFSET, 1) $ "Yy", .T., .F.)

   // Process report eject and plain attributes option byte
   cOptionByte = ASC(SUBSTR(cParamsBuff, OPTION_OFFSET, 1))

   IF INT(cOptionByte / 4) = 1
      aReport[ RP_PLAIN ] := .T.          // Plain page
      cOptionByte -= 4
   ENDIF

   IF INT(cOptionByte / 2) = 1
      aReport[ RP_AEJECT ] := .T.         // Page eject after report
      cOptionByte -= 2
   ENDIF

   IF INT(cOptionByte / 1) = 1
      aReport[ RP_BEJECT ] := .F.         // Page eject before report
      cOptionByte -= 1
   ENDIF

   // Page heading, report title
   nPointer = BIN2W(SUBSTR(cParamsBuff, PAGE_HDR_OFFSET, 2))

  aReport[ RP_HEADER ] := ;
    ListAsArray(GetExpr( nPointer ),";",aReport[ RP_WIDTH ] - aReport[ RP_RMARGIN ] )


   // Process Groups
   // Group
   nPointer = BIN2W(SUBSTR(cParamsBuff, GRP_EXPR_OFFSET, 2))

   IF !EMPTY(cGroupExp := GetExpr( nPointer ))

      // Add a new group array
      AADD( aReport[ RP_GROUPS ], ARRAY( RG_COUNT ))

      // Group expression
      aReport[ RP_GROUPS ][1][ RG_TEXT ] := cGroupExp
      aReport[ RP_GROUPS ][1][ RG_EXP ] := &( "{ || " + cGroupExp + "}" )
      IF USED()
         aReport[ RP_GROUPS ][1][ RG_TYPE ] := ;
                        VALTYPE( EVAL( aReport[ RP_GROUPS ][1][ RG_EXP ] ) )
      ENDIF

      // Group header
      nPointer = BIN2W(SUBSTR(cParamsBuff, GRP_HDR_OFFSET, 2))
      aReport[ RP_GROUPS ][1][ RG_HEADER ] := GetExpr( nPointer )

      // Page eject after group
      aReport[ RP_GROUPS ][1][ RG_AEJECT ] := IF(SUBSTR(cParamsBuff, PE_OFFSET, 1) $ "Yy", .T., .F.)
      
   ENDIF

   // Subgroup
   nPointer = BIN2W(SUBSTR(cParamsBuff, SUB_EXPR_OFFSET, 2))

   IF !EMPTY(cSubGroupExp := GetExpr( nPointer ))

      // Add new group array
      AADD( aReport[ RP_GROUPS ], ARRAY( RG_COUNT ))

      // Subgroup expression
      aReport[ RP_GROUPS ][2][ RG_TEXT ] := cSubGroupExp
      aReport[ RP_GROUPS ][2][ RG_EXP ] := &( "{ || " + cSubGroupExp + "}" )
      IF USED()
         aReport[ RP_GROUPS ][2][ RG_TYPE ] := ;
                        VALTYPE( EVAL( aReport[ RP_GROUPS ][2][ RG_EXP ] ) )
      ENDIF

      // Subgroup header
      nPointer = BIN2W(SUBSTR(cParamsBuff, SUB_HDR_OFFSET, 2))
      aReport[ RP_GROUPS ][2][ RG_HEADER ] := GetExpr( nPointer )

      // Page eject after subgroup
      aReport[ RP_GROUPS ][2][ RG_AEJECT ] := .F.

   ENDIF

   // Process columns
   nFieldOffset := 12      // dBASE skips first 12 byte fields block.
   FOR nCount := 1 to nColCount

      AADD( aReport[ RP_COLUMNS ], GetColumn( cFieldsBuff, @nFieldOffset ) )

   NEXT

ENDIF

RETURN aReport


/***
*  GetExpr( nPointer ) --> cString
*
*  Reads an expression from EXPR_BUFF via the OFFSETS_BUFF and returns
*  a pointer to offset contained in OFFSETS_BUFF that in turn points
*  to an expression located in the EXPR_BUFF string.
*
*  Notes:
*
*     1. The expression is empty if:
*         a. Passed pointer is equal to 65535
*         b. Character following character pointed to by pointer is CHR(0)
*             
*/
STATIC FUNCTION GetExpr( nPointer )
   LOCAL nExprOffset   := 0
   LOCAL nExprLength   := 0
   LOCAL nOffsetOffset := 0
   LOCAL cString := ""

   // Stuff for dBASE compatability.
   IF nPointer != 65535

      // Convert DOS FILE offset to CLIPPER string offset
      nPointer++

      // Calculate offset into OFFSETS_BUFF
      IF nPointer > 1
         nOffsetOffset = (nPointer * 2) - 1
      ENDIF

      nExprOffset = BIN2W(SUBSTR(cOffsetsBuff, nOffsetOffset, 2))
      nExprLength = BIN2W(SUBSTR(cLengthsBuff, nOffsetOffset, 2))

      // EXPR_OFFSET points to a NULL, so add one (+1) to get the string
      // and subtract one (-1) from EXPR_LENGTH for correct length

      nExprOffset++
      nExprLength--

      // Extract string
      cString = SUBSTR(cExprBuff, nExprOffset, nExprLength)

      // dBASE does this so we must do it too
      // Character following character pointed to by pointer is NULL
      IF Asc(SUBSTR(cString, 1, 1))=0 .AND. LEN(SUBSTR(cString,1,1)) = 1
         cString = ""
      ENDIF
   ENDIF

   RETURN (cString)


/***
*  GetColumn( <cFieldBuffer>, @<nOffset> ) --> aColumn
*
*  Get a COLUMN element from FIELDS_BUFF string using nOffset to point to
*  the current FIELDS_OFFSET block.
*
*  Notes:
*     1. The Header or Contents expressions are empty if:
*        a. Passed pointer is equal to 65535
*        b. Character following character pointed to by pointer is CHR(0)
*
*/
STATIC FUNCTION GetColumn( cFieldsBuffer, nOffset )
   LOCAL nPointer := 0, nNumber := 0, aColumn[ RC_COUNT ], cType

   // Column width
   aColumn[ RC_WIDTH ] := BIN2W(SUBSTR(cFieldsBuffer, nOffset + FIELD_WIDTH_OFFSET, 2))

   // Total column?
   aColumn[ RC_TOTAL ] := IF(SUBSTR(cFieldsBuffer, nOffset + FIELD_TOTALS_OFFSET, 1) $ "Yy", .T., .F.)
   
   // Decimals width
   aColumn[ RC_DECIMALS ] := BIN2W(SUBSTR(cFieldsBuffer, nOffset + FIELD_DECIMALS_OFFSET, 2))

   // Offset (relative to FIELDS_OFFSET), 'point' to
   // expression area via array OFFSETS[]
   
   // Content expression
   nPointer = BIN2W(SUBSTR(cFieldsBuffer, nOffset +;
               FIELD_CONTENT_EXPR_OFFSET, 2))
   aColumn[ RC_TEXT ] := GetExpr( nPointer )
   aColumn[ RC_EXP ] := &( "{ || " + GetExpr( nPointer ) + "}" )
   
   // Header expression
   nPointer = BIN2W(SUBSTR(cFieldsBuffer, nOffset +;
               FIELD_HEADER_EXPR_OFFSET, 2))

   aColumn[ RC_HEADER ] := ListAsArray(GetExpr( nPointer ), ";")
   
   // Column picture
   // Setup picture only if a database file is open
   IF USED()
      cType := VALTYPE( EVAL(aColumn[ RC_EXP ]) )
      aColumn[ RC_TYPE ] := cType
      DO CASE
      CASE cType = "C" .OR. cType = "M"
         aColumn[ RC_PICT ] := REPLICATE("X", aColumn[ RC_WIDTH ])
      CASE cType = "D"
         aColumn[ RC_PICT ] := "@D"
      CASE cType = "N"
         IF aColumn[ RC_DECIMALS ] != 0
            aColumn[ RC_PICT ] := REPLICATE("9", aColumn[ RC_WIDTH ] - aColumn[ RC_DECIMALS ] -1) + "." + ;
                                  REPLICATE("9", aColumn[ RC_DECIMALS ])
         ELSE
            aColumn[ RC_PICT ] := REPLICATE("9", aColumn[ RC_WIDTH ])
         ENDIF
      CASE cType = "L"
         aColumn[ RC_PICT ] := "@L" + REPLICATE("X",aColumn[ RC_WIDTH ]-1)
      ENDCASE
   ENDIF

   // Update offset into ?_buffer
   nOffset += 12

   RETURN ( aColumn )

/***
*  ListAsArray( <cList>, <cDelimiter>, [<nWidth>] ) --> aList
*  Convert a delimited string to an array
*
*
STATIC FUNCTION ListAsArray( cList, cDelimiter, nWidth )

LOCAL nPos
LOCAL aList := {}              // Define an empty array
LOCAL lDelimLast := .f.
  
  IF cDelimiter = NIL
    cDelimiter := ","
  ENDIF

  if nWidth == NIL
    nWidth := Len(cList)
  end

  DO WHILE ( Len(cList) <> 0 )

    nPos := AT(cDelimiter, cList)

    if ( nPos == 0 )
      nPos := Len(cList)
    end

    if ( nPos - 1 > nWidth )
      nPos := nWidth

      while ( nPos > 0 .and. substr(cList, nPos, 1) <> " " )
        nPos --
      end

      if ( nPos == 0 )
        nPos := nWidth
      end
        end

    if ( SUBSTR( cList, nPos, 1 ) == cDelimiter )
      lDelimLast := .t.
      AADD(aList, SUBSTR(cList, 1, nPos - 1)) // Add a new element
    else
            lDelimLast := .f.
      AADD(aList, SUBSTR(cList, 1, nPos)) // Add a new element
    end

    cList := SUBSTR(cList, nPos + 1)

  ENDDO

  if ( lDelimLast )
    AADD(aList, "")
  end

RETURN aList                // Return the array
*/
STATIC FUNCTION ListAsArray( cList, cDelimiter, nWidth )

LOCAL nPos
LOCAL aList := {}              // Define an empty array
LOCAL lDelimLast := .f.
local txt
  
  IF cDelimiter = NIL
    cDelimiter := ","
  ENDIF

  if nWidth == NIL
    nWidth := Len(cList)
  end

  DO WHILE ( Len(cList) <> 0 )

    nPos := AT(cDelimiter, cList)

    if ( nPos == 0 )
      nPos := Len(cList)
    end

    if ( nPos - 1 > nWidth )
      nPos := nWidth

      while ( nPos > 0 .and. substr(cList, nPos, 1) <> " " )
        nPos --
      end

      if ( nPos == 0 )
        nPos := nWidth
      end
    end

    if ( SUBSTR( cList, nPos, 1 ) == cDelimiter )
      txt:= SUBSTR(cList, 1, nPos - 1)
    else
      txt:= SUBSTR(cList, 1, nPos )
    end

    if !empty(txt)
       aadd(aList,txt)
    endif

    cList := SUBSTR(cList, nPos + 1)

  ENDDO

RETURN aList                // Return the array

