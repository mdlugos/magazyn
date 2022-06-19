#include "std.ch"
#define PC852
#define isPrinter() .t.
#command initialize screen  => __run('uniznak.exe 8 r')
#command INITIALIZE PRINTER => if(type("MEMVAR->p_init")="C",qqout(MEMVAR->p_init),)
#define A_DRUKCOMP
#define A_WADO
#define A_XPRN
#define A_15CPI
#define A_STYLUS
#define A_DLINK
#define A_SET_DAT GERMAN
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define DatE() MEMVAR->dzisiaj
#define A_WA
#define A_WE
#define A_SWWKTM
#define STANY indx_mat
#define A_SUMK    -55140562141
#define A_KOMU_N  'Urz¥d Wojew¢dzki w Katowicach - Wydziaˆ Administracyjno-Gospodarczy'
#define A_KOMU_A  'Katowice, ul. Jagielloäska 25'
#define A_AUTOR   "A.D. 1998, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)8524048"
#define A_FILELIMIT '41'
#define A_NAZBEG 5
#define A_NRLTH 3
#define A_NRLTH3
#define INDEXPIC "XXXXXXXXXXXX"
#define A_LFIRST
#define A_FIFO
//#define A_MYSZ
