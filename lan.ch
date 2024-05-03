#ifdef __HARBOUR__
        #define mkdir(x) makedir(x)
        #command SET RDD DEFAULT [TO] <x> => REQUEST <x>;rddsetdefault(<"x">)
        #command @ <top>, <left>, <bottom>, <right> BOXB <string> [COLOR <color>] => @ <top>, <left>, <bottom>, <right> BOX HB_UTF8TOSTRBOX(HB_TRANSLATE(<string>,'PLMAZ','UTF8')) [COLOR <color>]
        #command @ <row>, <col> SAYB <xpr> [COLOR <color>] [CODEPAGE <cdp>] => pushCDP([<cdp>]); @ <row>, <col> SAY <xpr> [COLOR <color>] ; popCDP()
#else
#define MSHOW() sysint(51,1)
#define MHIDE() sysint(51,2)
        #ifndef _SET_DEFINED
        #include "Set.ch"
        #endif
        #command DO WHILE <exp> => while <exp>
        #command END <x> => end
        #command NEXT <v> [TO <x>] [STEP <s>] => next
        #command STORE <value> TO <var1> [, <varN> ] => <var1> := [ <varN> := ] <value>
        #command SET EXACT <x:ON,OFF,&> => Set( _SET_EXACT, <(x)> )
        #command SET EXACT (<x>) => Set( _SET_EXACT, <x> )
        #command SET FIXED <x:ON,OFF,&> => Set( _SET_FIXED, <(x)> )
        #command SET FIXED (<x>) => Set( _SET_FIXED, <x> )
        #command SET DECIMALS TO <x> => Set( _SET_DECIMALS, <x> )
        #command SET DECIMALS TO => Set( _SET_DECIMALS, 0 )
        #command SET PATH TO <*path*> => Set( _SET_PATH, <(path)> )
        #command SET PATH TO => Set( _SET_PATH, "" )
        #command SET DEFAULT TO <(path)> => Set( _SET_DEFAULT, <(path)> )
        #command SET DEFAULT TO => Set( _SET_DEFAULT, "" )
        #command SET CENTURY <x:ON,OFF,&> => __SetCentury( <(x)> )
        #command SET CENTURY (<x>) => __SetCentury( <x> )
        #command SET EPOCH TO <year> => Set( _SET_EPOCH, <year> )
        #command SET DATE FORMAT [TO] <c> => Set( _SET_DATEFORMAT, <c> )
        #define _DFSET(x, y) Set( _SET_DATEFORMAT, if(__SetCentury(), x, y) )
        #command SET DATE [TO] ANSI => _DFSET( "yyyy.mm.dd", "yy.mm.dd" )
        #command SET DATE [TO] GERMAN => _DFSET( "dd.mm.yyyy", "dd.mm.yy" )
        #undef  _DFSET
        #command SET ALTERNATE <x:ON,OFF,&> => Set( _SET_ALTERNATE, <(x)> )
        #command SET ALTERNATE (<x>) => Set( _SET_ALTERNATE, <x> )
        #command SET ALTERNATE TO => Set( _SET_ALTFILE, "" )
        #command SET ALTERNATE TO <(file)> [<add: ADDITIVE>] => Set( _SET_ALTFILE, <(file)>, <.add.> )
        #command SET CONSOLE <x:ON,OFF,&> => Set( _SET_CONSOLE, <(x)> )
        #command SET CONSOLE (<x>) => Set( _SET_CONSOLE, <x> )
        #command SET MARGIN TO <x> => Set( _SET_MARGIN, <x> )
        #command SET MARGIN TO => Set( _SET_MARGIN, 0 )
        #command SET PRINTER <x:ON,OFF,&> => Set( _SET_PRINTER, <(x)> )
        #command SET PRINTER (<x>) => Set( _SET_PRINTER, <x> )
        #command SET PRINTER TO => Set( _SET_PRINTFILE, "" )
        #command SET PRINTER TO <(file)> [<add: ADDITIVE>] => Set( _SET_PRINTFILE, <(file)>, <.add.> )
        #command SET DEVICE TO SCREEN => Set( _SET_DEVICE, "SCREEN" )
        #command SET DEVICE TO PRINTER => Set( _SET_DEVICE, "PRINTER" )
        #command SET COLOR TO [<*spec*>] => SetColor( #<spec> )
        #command SET COLOR TO ( <c> ) => SetColor( <c> )
        #command SET CURSOR <x:ON,OFF,&> => SetCursor( if(Upper(<(x)>) == "ON", 1, 0) )
        #command SET CURSOR (<x>) => SetCursor( if(<x>, 1, 0) )
        #command ? [ <list,...> ] => QOut( <list> )
        #command ?? [ <list,...> ] => QQOut( <list> )
        #command EJECT => __Eject()
        #command CLS => Scroll() ; SetPos(0,0)
        #command CLEAR SCREEN => CLS
        #command @ <row>, <col> => Scroll( <row>, <col>, <row> ) ; SetPos( <row>, <col> )
        #command @ <top>, <left> CLEAR => Scroll( <top>, <left> ) ; SetPos( <top>, <left> )
        #command @ <top>, <left> CLEAR TO <bottom>, <right> => Scroll( <top>, <left>, <bottom>, <right> ) ; SetPos( <top>, <left> )
        #command @ <top>, <left>, <bottom>, <right> BOX <string> [COLOR <color>];
         => DispBox( <top>, <left>, <bottom>, <right>, <string> [, <color> ] )
        #command @ <top>, <left> TO <bottom>, <right> [DOUBLE] [COLOR <color>] => DispBox( <top>, <left>, <bottom>, <right>, 2 [, <color> ] )
        #command @ <top>, <left> TO <bottom>, <right> [COLOR <color>] => DispBox( <top>, <left>, <bottom>, <right>, 1 [, <color> ] )
        #command @ <row>, <col> SAY <xpr> [PICTURE <pic>] [COLOR <color>] => SetPos( <row>, <col> ) ; DevOutPict(<xpr>, <pic> [, <color>] )
        #command @ <row>, <col> SAY <xpr> [COLOR <color>] => SetPos( <row>, <col> ) ; DevOut( <xpr> [, <color>] )
        #command @ <row>, <col> SAYB <xpr> [COLOR <color>] => SetPos( <row>, <col> ) ; DevOut( <xpr> [, <color>] )
        #command SET BELL <x:ON,OFF,&> => Set( _SET_BELL, <(x)> )
        #command SET BELL (<x>) => Set( _SET_BELL, <x> )
        #command SET CONFIRM <x:ON,OFF,&> => Set( _SET_CONFIRM, <(x)> )
        #command SET CONFIRM (<x>) => Set( _SET_CONFIRM, <x> )
        #command SET ESCAPE <x:ON,OFF,&> => Set( _SET_ESCAPE, <(x)> )
        #command SET ESCAPE (<x>) => Set( _SET_ESCAPE, <x> )
        #command SET INTENSITY <x:ON,OFF,&> => Set( _SET_INTENSITY, <(x)> )
        #command SET INTENSITY (<x>) => Set( _SET_INTENSITY, <x> )
        #command SET SCOREBOARD <x:ON,OFF,&> => Set( _SET_SCOREBOARD, <(x)> )
        #command SET SCOREBOARD (<x>) => Set( _SET_SCOREBOARD, <x> )
        #command SET DELIMITERS <x:ON,OFF,&> => Set( _SET_DELIMITERS, <(x)> )
        #command SET DELIMITERS (<x>) => Set( _SET_DELIMITERS, <x> )
        #command SET DELIMITERS TO <c> => Set( _SET_DELIMCHARS, <c> )
        #command SET DELIMITERS TO DEFAULT => Set( _SET_DELIMCHARS, "::" )
        #command SET DELIMITERS TO => Set( _SET_DELIMCHARS, "::" )
        #command @ <row>, <col> GET <var> [PICTURE <pic>] [VALID <valid>] [WHEN <when>] [SEND <msg>] => SetPos( <row>, <col> ) ; AAdd(GetList,_GET_( <var>, <"var">, <pic>, <{valid}>, <{when}> ):display()) [; ATail(GetList):<msg>]
        #command @ <row>, <col> SAY <sayxpr> [<sayClauses,...>] GET <var> [<getClauses,...>] => @ <row>, <col> SAY <sayxpr> [<sayClauses>] ; GETL <var> [<getClauses>]
        #command @ <row>, <col> GET <var> [<clauses,...>] RANGE <lo>, <hi> [<moreClauses,...>] => @ <row>, <col> GET <var> [<clauses>] VALID {|_1| RangeCheck(_1,, <lo>, <hi>)} [<moreClauses>]
        #command @ <row>, <col> GET <var> [<clauses,...>] COLOR <color> [<moreClauses,...>] => @ <row>, <col> GET <var> [<clauses>] SEND colorDisp(<color>) [<moreClauses>]
        #command SET WRAP <x:ON,OFF,&> => Set( _SET_WRAP, <(x)> )
        #command SET WRAP (<x>) => Set( _SET_WRAP, <x> )
        #command SET MESSAGE TO <n> [<cent: CENTER, CENTRE>] => Set( _SET_MESSAGE, <n> ) ; Set( _SET_MCENTER, <.cent.> )
        #command SET MESSAGE TO => Set( _SET_MESSAGE, 0 ) ; Set( _SET_MCENTER, .f. )
        #command @ <row>, <col> PROMPT <prompt> [MESSAGE <msg>] => __AtPrompt( <row>, <col>, <prompt> , <msg> )
        #command MENU TO <v> => <v> := __MenuTo( {|_1| if(PCount() == 0, <v>, <v> := _1)}, #<v> )
        #command SAVE SCREEN => __XSaveScreen()
        #command RESTORE SCREEN => __XRestScreen()
        #command SAVE SCREEN TO <var> => <var> := SaveScreen( 0, 0, Maxrow(), Maxcol() )
        #command RESTORE SCREEN FROM <c> => RestScreen( 0, 0, Maxrow(), Maxcol(), <c> )
        #command WAIT [<c>] => __Wait( <c> )
        #command WAIT [<c>] TO <var> => <var> := __Wait( <c> )
        #command ACCEPT [<c>] TO <var> => <var> := __Accept( <c> )
        #command INPUT [<c>] TO <var> => if ( !Empty(__Accept(<c>)) ) ; <var> := &( __AcceptStr() ) ; endif
        #command KEYBOARD <c> => __Keyboard( <c> )
        #command CLEAR TYPEAHEAD => __Keyboard()
        #command SET TYPEAHEAD TO <n> => Set( _SET_TYPEAHEAD, <n> )
        #command SET KEY <n> TO <proc> => SetKey( <n>, {|p, l, v| <proc>(p, l, v)} )
        #command SET KEY <n> TO <proc:&> => if ( Empty(<(proc)>) ) ; SetKey( <n>, NIL ) ; else ; SetKey( <n>, {|p, l, v| <proc>(p, l, v)} ) ; endif
        #command SET KEY <n> [TO] => SetKey( <n>, NIL )
        #command CLEAR MEMORY => __MClear()
        #command RELEASE <vars,...> => __MXRelease( <"vars"> )
        #command RELEASE ALL => __MRelease("*", .t.)
        #command RELEASE ALL LIKE <skel> => __MRelease( #<skel>, .t. )
        #command RELEASE ALL EXCEPT <skel> => __MRelease( #<skel>, .f. )
        #command RESTORE [FROM <(file)>] [<add: ADDITIVE>] => __MRestore( <(file)>, <.add.> )
        #command SAVE ALL LIKE <skel> TO <(file)> => __MSave( <(file)>, <(skel)>, .t. )
        #command SAVE TO <(file)> ALL LIKE <skel> => __MSave( <(file)>, <(skel)>, .t. )
        #command SAVE ALL EXCEPT <skel> TO <(file)> => __MSave( <(file)>, <(skel)>, .f. )
        #command SAVE TO <(file)> ALL EXCEPT <skel> => __MSave( <(file)>, <(skel)>, .f. )
        #command SAVE [TO <(file)>] [ALL] => __MSave( <(file)>, "*", .t. )
        #command ERASE <(file)> => FErase( <(file)> )
        #command DELETE FILE <(file)> => FErase( <(file)> )
        #command RENAME <(old)> TO <(new)> => FRename( <(old)>, <(new)> )
        #command COPY FILE <(src)> TO <(dest)> => __CopyFile( <(src)>, <(dest)> )
        #command DIR [<(spec)>] => __Dir( <(spec)> )
        #command TYPE <(file)> [<print: TO PRINTER>] [TO FILE <(dest)>] => __TypeFile( <(file)>, <.print.> ) [; COPY FILE <(file)> TO <(dest)> ]
        #command TYPE <(file)> [<print: TO PRINTER>] => __TypeFile( <(file)>, <.print.> )
        #command REQUEST <vars,...> => EXTERNAL <vars>
        #command CANCEL => __Quit()
        #command QUIT => __Quit()
        #command RUN <*cmd*> => __Run( #<cmd> )
        #command RUN ( <c> ) => __Run( <c> )
        #command ! <*cmd*> => RUN <cmd>
        #command RUN = <xpr> => ( run := <xpr> )
        #command RUN := <xpr> => ( run := <xpr> )
        #command SET EXCLUSIVE <x:ON,OFF,&> => Set( _SET_EXCLUSIVE, <(x)> )
        #command SET EXCLUSIVE (<x>) => Set( _SET_EXCLUSIVE, <x> )
        #command SET SOFTSEEK <x:ON,OFF,&> => Set( _SET_SOFTSEEK, <(x)> )
        #command SET SOFTSEEK (<x>) => Set( _SET_SOFTSEEK, <x> )
        #command SET UNIQUE <x:ON,OFF,&> => Set( _SET_UNIQUE, <(x)> )
        #command SET UNIQUE (<x>) => Set( _SET_UNIQUE, <x> )
        #command SET DELETED <x:ON,OFF,&> => Set( _SET_DELETED, <(x)> )
        #command SET DELETED (<x>) => Set( _SET_DELETED, <x> )
        #command SELECT <whatever> => dbSelectArea( <(whatever)> )
        #command SELECT <f>([<list,...>]) => dbSelectArea( <f>(<list>) )
        #command USE => dbCloseArea()
        #command USE <(db)> [VIA <rdd>] [ALIAS <a>] [<new: NEW>] [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] [INDEX <(index1)> [, <(indexn)>]] ;
         => dbUseArea( <.new.>, <rdd>, <(db)>, <(a)>,if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.> ) [; ordlistadd( <(index1)> )] [; ordlistadd( <(indexn)> )]
        #command APPEND BLANK => dbAppend()
        #command PACK => __dbPack()
        #command ZAP => __dbZap()
        #command GOTO <n> => dbGoto(<n>)
        #command GO <n> => dbGoto(<n>)
        #command GOTO TOP => dbGoTop()
        #command GO TOP => dbGoTop()
        #command GOTO BOTTOM => dbGoBottom()
        #command GO BOTTOM => dbGoBottom()
        #command SKIP => dbSkip(1)
        #command SKIP <n> => dbSkip( <n> )
        #command SKIP ALIAS <a> => <a> -> ( dbSkip(1) )
        #command SKIP <n> ALIAS <a> => <a> -> ( dbSkip(<n>) )
        #command SEEK <xpr> [<soft: SOFTSEEK>] => dbSeek( <xpr>, if( <.soft.>, .T., NIL ) )
        #command CONTINUE => __dbContinue()
        #command LOCATE [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL] ;
              => __dbLocate( <{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command SET RELATION TO => dbClearRel()
        #command SET RELATION [<add:ADDITIVE>] [TO <key1> INTO <(alias1)> [, [TO] <keyn> INTO <(aliasn)>]] ;
              => if ( !<.add.> );dbClearRel(); endif ; dbSetRelation( <(alias1)>, <{key1}>, <"key1"> ) [; dbSetRelation( <(aliasn)>, <{keyn}>, <"keyn"> )]
        #command SET FILTER TO => dbClearFilter(NIL)
        #command SET FILTER TO <xpr> => dbSetFilter( <{xpr}>, <"xpr"> )
        #command SET FILTER TO <x:&> => if ( Empty(<(x)>) );dbClearFilter(); else;dbSetFilter( <{x}>, <(x)> ) ; endif
        #command REPLACE [ <f1> WITH <x1> [, <fn> WITH <xn>] ] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL] ;
              => DBEval( {|| _FIELD-><f1> := <x1> [, _FIELD-><fn> := <xn>]}, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command REPLACE <f1> WITH <v1> [, <fN> WITH <vN> ] => _FIELD-><f1> := <v1> [; _FIELD-><fN> := <vN>]
        #command DELETE [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL] ;
              => DBEval({|| dbDelete()},<{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command RECALL [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL] => DBEval( {|| dbRecall()}, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command DELETE => dbDelete()
        #command RECALL => dbRecall()
        #command CREATE <(file1)> [FROM <(file2)>] => __dbCreate( <(file1)>, <(file2)> )
        #command COPY [STRUCTURE] [EXTENDED] [TO <(file)>] => __dbCopyXStruct( <(file)> )
        #command COPY [STRUCTURE] [TO <(file)>] [FIELDS <fields,...>] => __dbCopyStruct( <(file)>, { <(fields)> } )
        #command COPY [TO <(file)>] [DELIMITED [WITH <*delim*>]] [FIELDS <fields,...>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL];
         => __dbDelim(.t., <(file)>, <(delim)>, { <(fields)> }, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command COPY [TO <(file)>] [SDF] [FIELDS <fields,...>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL] => __dbSDF(.t., <(file)>, { <(fields)> }, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command COPY [TO <(file)>] [FIELDS <fields,...>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL] => __dbCopy( <(file)>, { <(fields)> }, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command APPEND [FROM <(file)>] [DELIMITED [WITH <*delim*>]] [FIELDS <fields,...>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL];
         => __dbDelim(.f., <(file)>, <(delim)>, { <(fields)> }, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command APPEND [FROM <(file)>] [SDF] [FIELDS <fields,...>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL] => __dbSDF(.f., <(file)>, { <(fields)> }, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command APPEND [FROM <(file)>] [FIELDS <fields,...>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL] => __dbApp( <(file)>, { <(fields)> }, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command SORT [TO <(file)>] [ON <fields,...>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL] => __dbSort( <(file)>, { <(fields)> }, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command TOTAL [TO <(file)>] [ON <key>] [FIELDS <fields,...>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL] => __dbTotal( <(file)>, <{key}>, { <(fields)> }, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command UPDATE [FROM <(alias)>] [ON <key>] [REPLACE <f1> WITH <x1> [, <fn> WITH <xn>]] [<rand:RANDOM>] => __dbUpdate( <(alias)>, <{key}>, <.rand.>, {|| _FIELD-><f1> := <x1> [, _FIELD-><fn> := <xn>]} )
        #command JOIN [WITH <(alias)>] [TO <file>] [FIELDS <fields,...>] [FOR <for>] => __dbJoin( <(alias)>, <(file)>, { <(fields)> }, <{for}> )
        #command COUNT [TO <var>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL] => <var> := 0 ; DBEval( {|| <var> := <var> + 1}, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command SUM [ <x1> [, <xn>] TO <v1> [, <vn>] ] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL];
         => <v1> := [ <vn> := ] 0 ; DBEval( {|| <v1> += <x1> [, <vn> += <xn> ]}, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
        #command AVERAGE [ <x1> [, <xn>] TO <v1> [, <vn>] ] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL];
         => M->__Avg := <v1> := [ <vn> := ] 0 ; DBEval( {|| M->__Avg := M->__Avg + 1, <v1> := <v1> + <x1> [, <vn> := <vn> + <xn>] }, <{for}>, <{while}>, <next>, <rec>, <.rest.> ) ; <v1> := <v1> / M->__Avg [; <vn> := <vn> / M->__Avg ]
        #command LIST [<list,...>] [<off:OFF>] [<toPrint: TO PRINTER>] [TO FILE <(toFile)>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL];
         => __dbList( <.off.>, { <{list}> }, .t., <{for}>, <{while}>, <next>, <rec>, <.rest.>, <.toPrint.>, <(toFile)> )
        #command DISPLAY [<list,...>] [<off:OFF>] [<toPrint: TO PRINTER>] [TO FILE <(toFile)>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [<all:ALL>];
         => __DBList( <.off.>, { <{list}> }, <.all.>, <{for}>, <{while}>, <next>, <rec>, <.rest.>, <.toPrint.>, <(toFile)> )
        #command REPORT FORM <frm> [HEADING <heading>] [<plain: PLAIN>] [<noeject: NOEJECT>] [<summary: SUMMARY>] [<noconsole: NOCONSOLE>] [<print: TO PRINTER>] [TO FILE <(toFile)>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL];
         => __ReportForm( <(frm)>, <.print.>, <(toFile)>, <.noconsole.>, <{for}>, <{while}>, <next>, <rec>, <.rest.>, <.plain.>, <heading>, <.noeject.>, <.summary.> )
        #command LABEL FORM <lbl> [<sample: SAMPLE>] [<noconsole: NOCONSOLE>] [<print: TO PRINTER>] [TO FILE <(toFile)>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL];
         => __LabelForm( <(lbl)>, <.print.>, <(toFile)>, <.noconsole.>, <{for}>, <{while}>, <next>, <rec>, <.rest.>, <.sample.> )
        #command CLOSE <alias> => <alias>->( dbCloseArea() )
        #command CLOSE => dbCloseArea()
        #command CLOSE DATABASES => dbCloseAll()
        #command CLOSE ALTERNATE => Set(_SET_ALTFILE, "")
        #command CLOSE INDEXES => dbClearIndex()
        #command CLOSE PROCEDURE =>
        #command CLOSE ALL => CLOSE DATABASES ; SELECT 1
        #command CLEAR => CLEAR SCREEN
        #command CLEAR ALL => CLOSE DATABASES ; CLEAR MEMORY ; SET ALTERNATE OFF ; SET ALTERNATE TO
        #command INDEX ON <key> [TAG <(cOrderName)> ] TO <(cOrderBagName)> [FOR <for>] [<all:ALL>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [EVAL <eval>] [EVERY <every>] [<unique: UNIQUE>] [<ascend: ASCENDING>] [<descend: DESCENDING>] ;
              => ordCondSet( <"for">, <{for}>,[<.all.>],<{while}>,<{eval}>, <every>,RECNO(), <next>, <rec>,[<.rest.>], [<.descend.>] );
              ;  ordCreate(<(cOrderBagName)>, <(cOrderName)>,<"key">, <{key}>, [<.unique.>] )
        #command INDEX ON <key> TAG <(cOrderName)> [TO <(cOrderBagName)>] [FOR <for>] [<all:ALL>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [EVAL <eval>] [EVERY <every>] [<unique: UNIQUE>] [<ascend: ASCENDING>] [<descend: DESCENDING>] ;
              => ordCondSet( <"for">, <{for}>,[<.all.>],<{while}>,<{eval}>, <every>,RECNO(), <next>, <rec>,[<.rest.>], [<.descend.>] );
              ;  ordCreate(<(cOrderBagName)>, <(cOrderName)>,<"key">, <{key}>, [<.unique.>] )
        #command INDEX ON <key> TO <(file)> [<u: UNIQUE>] ;
              => dbCreateIndex(<(file)>, <"key">, <{key}>,if( <.u.>, .t., NIL ))
        #command DELETE TAG <(cOrdName1)> [ IN <(cOrdBag1)> ] [, <(cOrdNameN)> [ IN <(cOrdBagN)> ] ] => ordDestroy( <(cOrdName1)>, <(cOrdBag1)> ) [; ordDestroy( <(cOrdNameN)>, <(cOrdBagN)> ) ]
        #command REINDEX [EVAL <eval>] [EVERY <every>] => ordCondSet(,,,, <{eval}>, <every>,,,,,,,) ; ordListRebuild()
        #command REINDEX => ordListRebuild()
        #command SET INDEX TO [ <(index1)> [, <(indexn)>]] [<add: ADDITIVE>] ;
              => if !<.add.> ; ordListClear() ; endif [; ordListAdd( <(index1)> )] [; ordListAdd( <(indexn)> )]
        #command SET ORDER TO <xOrder> [IN <(cOrdBag)>] => ordSetFocus( <xOrder> [, <(cOrdBag)>] )
        #command SET ORDER TO TAG <(cOrder)> [IN <(cOrdBag)>] => ordSetFocus( <(cOrder)> [, <(cOrdBag)>] )
        #command SET ORDER TO => ordSetFocus(0)

        #command SET RDD DEFAULT [TO] <x> => EXTERNAL <x>;rddsetdefault(<"x">)

#endif

#ifdef __PLATFORM__UNIX
#define HB_OsPathSeparator() "/"
#define HB_ps() "/"
#define HB_OsPathListSeparator() ":"
#define HB_OsDriveSeparator() ""
#define HB_EOL() chr(10)
#else
#define HB_OsPathSeparator() "\"
#define HB_ps() "\"
#define HB_OsPathListSeparator() ";"
#define HB_OsDriveSeparator() ":"
#define HB_EOL() chr(13)+chr(10)
#endif

#ifdef SIMPLE
 #include "simpleio.ch"
#endif

#command READ [POSITION <pos>] SAVE => __SetProc(0) ; ReadModal(GetList[,<pos>])
#command READ [POSITION <pos>] => __SetProc(0) ; ReadModal(GetList[,<pos>]) ; GetList := {}
#command XSELECT <(db)> [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro:READONLY>] [ORDER] [TAG [TO] <(order)>] => sel(<(db)>,<(order)>,if(<.sh.> .or. <.ex.>, !<.ex.>, NIL),<.ro.>)
#define XOR(x,y) if(x,!(y),y)
#command SAYL <sayxpr> [<sayClauses,...>] => @ Row(), Col()+1 SAY <sayxpr> [<sayClauses>]
#command SAYL <sayxpr> [<sayClauses,...>] GET <var> [<getClauses,...>] => @ Row(), Col()+1 SAY <sayxpr> [<sayClauses>] ; GETL <var> [<getClauses>]
#command GETL <var> [<getClauses,...>] => @ Row(), Col()+1 GET <var> [<getClauses>]
#command EXECUTE [<block>] [FOR <for>] [WHILE <while>] [NEXT <next>] [RECORD <rec>] [<rest:REST>] [ALL] => DBEval( <{block}>, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
#define A_ZAOKR 2
#command DEFAULT <x> TO <y> => IF (<x>)=NIL;<x>:=<y>;ENDIF

#define A_LAN

#command LOCK [<ident>] [IN <alias>] [NO BREAK] [LOOP] [MESSAGE <m>] => if ! [<alias>->](reclock(.t.,<m>,.f.,,<ident>));loop;endif
#command LOCK [<ident>] [IN <alias>] [LOOP] [MESSAGE <m>] => if ! [<alias>->](reclock(.t.,<m>,,,<ident>));loop;endif
#command LOCK [<ident>] [IN <alias>] [MESSAGE <m>] => [<alias>->](reclock(.f.,<m>,,,<ident>))
#command LOCK ALL [IN <(alias)>] [NO BREAK] [LOOP] [MESSAGE <m>] => if ! [<alias>->](filock(.t.,<m>,.f.,));loop;endif
#command LOCK ALL [IN <(alias)>] [LOOP] [MESSAGE <m>] => if ! [<alias>->](filock(.t.,<m>,,));loop;endif
#command LOCK ALL [IN <(alias)>] [MESSAGE <m>] => [<alias>->](filock(.f.,<m>,,))
#command UNLOCK [<ident>] [IN <alias>] => [<alias>->](dbrUnlock([<ident>]),dbcommit())
#command UNLOCK ALL => dbUnlockAll();dbcommitall()
#command COMMIT [IN <alias>] => [<alias>->](dbcommit())
#command COMMIT ALL => dbcommitall()

#ifdef A_NETIO
#command NUSE <(db)> [VIA <rdd>] [ALIAS <a>] [<new: NEW>] [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] [INDEX <(index1)> [, <(indexn)>]] ;
         => NetusE( <.new.>, <rdd>, <(db)>, <(a)>,if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.> ) [; ordlistadd( <(index1)> )] [; ordlistadd( <(indexn)> )]
#else
#ifdef A_SX
#command NUSE <(db)> [VIA <rdd>] [ALIAS <a>] [<new: NEW>] [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] [INDEX <(index1)> [, <(indexn)>]] ;
         => dbusearea( <.new.>, <rdd>, <(db)>, <(a)>,if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.> );IF DBINFO(132);DBINFO(131,A_SX);ENDIF [; ordlistadd( <(index1)> )] [; ordlistadd( <(indexn)> )]
#define NetusE(a,b,c,d,e,f) (dbusearea(a,b,c,d,e,f),if(DBINFO(132),DBINFO(131,A_SX),))
#else
#command NUSE <(db)> [VIA <rdd>] [ALIAS <a>] [<new: NEW>] [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] [INDEX <(index1)> [, <(indexn)>]] ;
         => dbUseArea( <.new.>, <rdd>, <(db)>, <(a)>,if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.> ) [; ordlistadd( <(index1)> )] [; ordlistadd( <(indexn)> )]
#define NetusE(a,b,c,d,e,f) dbusearea(a,b,c,d,e,f)
#endif
#endif
