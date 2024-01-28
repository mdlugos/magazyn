#include "lan.ch"
#define A_STOPKA 'Program: System Magazyn, '+wersja()+', producent: Firma Usˆug Informatycznych Marek Dˆugosz, 43-400 Cieszyn, ul. R¢wna 16'
#define PC852
#define PLWIN
#define UpP(x) UPPER(x)
#define A_CDX DBFCDX
#define A_KHNAZ 31
#define A_DOKFAK
#define A_KHSEP
#define A_ZLEC11
#command INIT SCREEN => //run('uniznak r')
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define A_STYLUS
#define A_XPRN
#define A_PCL
#define A_WIN_PRN .t.
#define A_PRINT(x) eval(MEMVAR->do_print,x)
#define A_GETLPT eval(MEMVAR->do_getlpt)
#define isPrinter() .t.
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_WA
#define A_WE
#define A_FIFO
#define A_NOZAP
#define A_SUMK    -37001590688
#define A_KOMU_N  'O˜rodek Leczniczo-Rehabilitacyjny "Bucze" Sp. z o.o.'
#define A_KOMU_A  'G¢rki Wielkie, ul. Harcerska 31'
#define A_AUTOR   "A.D. 2014, Marek Dˆugosz, Cieszyn ul. R¢wna 16, tel. 601842030"
#define DatE()    MEMVAR->dzisiaj
#define A_DRUKCOMP
#define A_SHORTIND
#define A_TRWALOSC
#define A_DIETA  .t. //alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define A_LFIRST
#define INDEXPIC "XXXX"
#define A_MYSZ
#define A_DLINK
#define A_LPNUM 3
#define A_WADO eval(memvar->podpisy)
#define A_SB
