#include "lan.ch"
//#define A_WAGI
//#define PROC_EN
#define A_DDBF
#define PC852
#ifdef __PLATFORM__Windows
#define A_CDX ADS
#else
#define A_CDX DBFCDX
#endif
//#define A_POLOWA
#define A_DODATKI //iložci posi’k¢w w zapotrzebowaniu i menu
#define A_GOCZ
#define UpP(x) UPPER(x)
#command INIT SCREEN => //run('uniznak.exe r')
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define A_GETLPT eval({|_f_|_f_:=getenv('TEMP')+'\',fclose(fcreateu(@_f_)),if(!"."$right(_f_,4),_f_+'.',_f_)})
#define A_PRINT(x) __run('dosprint '+x+' > nul' ); ferase(x)
#define A_DRUKCOMP
#define A_XPRN
#define A_STYLUS
#define A_PCL
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_DIETA  .t. // ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '45'
#define A_MYSZ
#define A_NOZAP
#define A_SUMK    -49121604675
#define A_KOMU_N  'Zakˆad Obsˆugi —l¥skiego Urz©du Wojew.'
#define A_KOMU_A  'Katowice, ul. Jagielloäska 25'
#define A_AUTOR   "A.D. 2004, Marek Dˆugosz, http://www.polbox.com/m/mdlugosz tel. 0601842030"
#define DatE()    MEMVAR->dzisiaj
#define A_LPNUM      // 2
