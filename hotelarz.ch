#include "lan.ch"
#define PC852
#define UpP(x) UPPER(x)
#define A_CDX DBFCDX
#define A_GOCZ
#define A_KHNAZ 31
#define A_DOKFAK
#define A_KHSEP
#command INIT SCREEN => run('uniznak r')
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define A_GETLPT eval({|_f_|_f_:=getenv('TEMP')+'\',fclose(fcreateu(@_f_)),if(!"."$right(_f_,4),_f_+'.',_f_)})
#define A_PRINT(x) __run(memvar->do_print+' '+x ); ferase(x)
#define A_STYLUS
#define A_XPRN
#define A_PCL
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_WA
#define A_WE
#define A_FIFO
#define A_NOZAP
#define A_SUMK   -45037471978
#define A_KOMU_N  'Zesp¢ˆ Szk¢ˆ Gastronomiczno-Hotelarskich'
#define A_KOMU_A  'Wisˆa, ul. Wˆadysˆawa Reymonta 2'
#define A_AUTOR   "A.D. 2004, Marek Dˆugosz, http://www.polbox.com/m/mdlugosz tel. 0601842030"
#define DatE()    MEMVAR->dzisiaj
#define A_DRUKCOMP
#define A_SHORTIND
#define A_TRWALOSC
#define A_DIETA   .t. //alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "####"
//#define A_MYSZ
#define A_DLINK
#define A_LPNUM len(pozycja)
#define A_WADO
#define A_SB
