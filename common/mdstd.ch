        #define mkdir(x) makedir(x)
        #command SET RDD DEFAULT [TO] <x> => REQUEST <x>;rddsetdefault(<"x">)
        #command @ <top>, <left>, <bottom>, <right> BOX UNICODE <string> [COLOR <color>] => @ <top>, <left>, <bottom>, <right> BOX HB_UTF8TOSTRBOX(<string>) [COLOR <color>]

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
#command LOCK [<ident>] [IN <alias>] [NO BREAK] [LOOP] [MESSAGE <m>] =>
#command UNLOCK [<ident>] [IN <alias>] => [<alias>->](dbcommit())
#command UNLOCK ALL => dbcommitall()
#command COMMIT [IN <alias>] => [<alias>->](dbcommit())
#command COMMIT ALL => dbcommitall()

#ifdef A_NETIO
#command NUSE <(db)> [VIA <rdd>] [ALIAS <a>] [<new: NEW>] [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] [INDEX <(index1)> [, <(indexn)>]] [CODEPAGE <cp>];
         => NetusE( <.new.>, <rdd>, <(db)>, <(a)>,if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.>, <cp> ) [; ordlistadd( <(index1)> )] [; ordlistadd( <(indexn)> )]
#else
#ifdef A_SX
#command NUSE <(db)> [VIA <rdd>] [ALIAS <a>] [<new: NEW>] [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] [INDEX <(index1)> [, <(indexn)>]] [CODEPAGE <cp>];
         => dbusearea( <.new.>, <rdd>, <(db)>, <(a)>,if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.>[,<cp>] );if DBINFO(132);DBINFO(131,A_SX);ENDIF [; ordlistadd( <(index1)> )] [; ordlistadd( <(indexn)> )]
#define NetusE(a,b,c,d,e,f,g) (dbusearea(a,b,c,d,e,f,g),if(DBINFO(132),DBINFO(131,A_SX),))
#else
#command NUSE <(db)> [VIA <rdd>] [ALIAS <a>] [<new: NEW>] [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] [INDEX <(index1)> [, <(indexn)>]] [CODEPAGE <cp>];
         => dbUseArea( <.new.>, <rdd>, <(db)>, <(a)>,if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.> [,<cp>]) [; ordlistadd( <(index1)> )] [; ordlistadd( <(indexn)> )]
#define NetusE(a,b,c,d,e,f,g) dbusearea(a,b,c,d,e,f,g)
#endif
#endif
