#include "lan.ch"
#define PC852
#define A_CDX DBFCDX
#define UpP(x) UPPER(x)
#define A_KHSEP
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_XPRN
#define A_PCL
#define A_15CPI
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define isPrinter (.t.)
#command init screen =>
#define STANY indx_mat
#define A_FIFO
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define A_WA
#define A_SET_DAT  GERMAN
#define A_DRUKCOMP
#define A_DOKCOMP 8
#define A_TRWALOSC
#define A_DIETA   .t. //alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define A_SWW "PKWiU:"
#define INDEXPIC "XXXXXX"
//#define A_LFIRST
#define A_MYSZ
#define A_DLINK
#define A_LPNUM 3
#define A_WADO
#define A_NOZAP
#define A_SB
#define A_SUMK    -43758561941
#define A_KOMU_N  'Korona Sanatorium Uzdrowiskowe'
#define A_KOMU_A  'Muszyna, ul. M˜ciwujewskiego 2'
#define A_AUTOR   'A.D. 2006, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 0-338522553'
#define A_BACKUP MEMVAR->backup
