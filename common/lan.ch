#ifdef A_UNICODE
#define hb_UTF8ToStr(x) x
#else
#define binfieldget(x) hb_FieldGet(x)
#define binfieldput(x,y) hb_FieldPut(x,y)
#endif

#define hb_UTF8ToStrBox(x) x

#define lTrim(x) x
#define sTR(x,y,z) hb_ntoC(x,z)
//#define stR(x,y) hb_ntoC(x,0)
#define sTr(x) hb_nToc(x)

#define HB_MACRO2STRING(x) [x]


#ifndef EvAlDb
  #define EvAlDb(x) &(x)
#endif
#ifndef EvaldB
  #define EvaldB eVal
#endif    

#command SET RDD DEFAULT [TO] <x> => REQUEST <x>;rddsetdefault(<"x">)
#command @ <top>, <left>, <bottom>, <right> BOX UNICODE <string> [COLOR <color>] => ;
         hb_DispBox(<top>, <left>, <bottom>, <right>, hb_UTF8ToStrBox(<string>)[, <color>])

#command @ <row>, <col> BOX <exp> UNICODE [COLOR <clr>] => ;
         hb_DispOutAtBox( <row>, <col>, hb_UTF8ToStrBox(<exp>) [, <clr>] )

#command @ <row>, <col> SAY <exp> UNICODE [PICTURE <pic>] [COLOR <clr>] => ;
         DevPos( <row>, <col> ) ; DevOutPict( hb_UTF8ToStr(<exp>), <pic> [, <clr>] )
#command @ <row>, <col> SAY <exp> UNICODE [COLOR <clr>] => ;
         DevPos( <row>, <col> ) ; DevOut( hb_UTF8ToStr(<exp>) [, <clr>] )
#command @ <row>, <col> PROMPT <prompt> UNICODE [MESSAGE <msg>] => ;
         __AtPrompt( <row>, <col>, hb_UTF8ToStr(<prompt>) [, hb_UTF8ToStr(<msg>) ])

#command MENU TO <v> SCREEN <s> => ;
  <v> := __MenuTo( {| _1 | iif( PCount() == 0, <v>, <v> := _1 ) }, #<v>, <s> )

#define mkdir(x) makedir(x)

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
#command NUSE <(db)> [VIA <rdd>] [ALIAS <a>] [<new: NEW>] [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] [CODEPAGE <cp>] [INDEX <(index1)> [, <(indexn)>]];
         => NetusE( <.new.>, <rdd>, <(db)>, <(a)>,if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.>[, <cp>] ) [; ordlistadd( <(index1)> )] [; ordlistadd( <(indexn)> )]
#else
#ifdef A_SX
#command NUSE <(db)> [VIA <rdd>] [ALIAS <a>] [<new: NEW>] [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] [CODEPAGE <cp>] [INDEX <(index1)> [, <(indexn)>]];
         => dbusearea( <.new.>, <rdd>, <(db)>, <(a)>,if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.>[,<cp>] );if DBINFO(132);DBINFO(131,A_SX);ENDIF [; ordlistadd( <(index1)> )] [; ordlistadd( <(indexn)> )]
#define NetusE(a,b,c,d,e,f,g) (dbusearea(a,b,c,d,e,f,g),if(DBINFO(132),DBINFO(131,A_SX),))
#else
#command NUSE <(db)> [VIA <rdd>] [ALIAS <a>] [<new: NEW>] [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] [CODEPAGE <(cp)>] [INDEX <(index1)> [, <(indexn)>]];
         => dbUseArea( <.new.>, <rdd>, <(db)>, <(a)>,if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.> [,<cp>]) [; ordlistadd( <(index1)> )] [; ordlistadd( <(indexn)> )]
#define NetusE(a,b,c,d,e,f,g) dbusearea(a,b,c,d,e,f,g)
#endif
#endif
