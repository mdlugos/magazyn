#include "std.ch"
#command INIT SCREEN =>
#command INIT PRINTER => qqout("@P")
#define isPrinter() .t.
#define DatE() MEMVAR->dzisiaj
#define A_DRUKCOMP
#define DTOV(dat) tranr(subs(dtos(dat),5),"##.##")
#define A_SET_DAT GERMAN
#define STANY   INDX_MAT
#define A_SUMK  -40351202515
#define A_KOMU_N  "W.O.R.R. Sanatourim Rehabilitacyjne Skocz¢w"
#define A_KOMU_A  "Skocz¢w, ul. Katowicka 21"
#define A_AUTOR   "A.D. 1992-1998, Marek D’ugosz, Cieszyn, ul. Korfantego 24, tel. 524048"
#define A_WA
//#define A_SHORTIND
#define A_SWW
#define A_TRWALOSC
#define A_DIETA   alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "##!###"
#define A_MYSZ
#define A_NOZAP
#define A_DLINK
