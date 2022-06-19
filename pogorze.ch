#include "lan.ch"
#define PC852
//#define PLWIN
#define A_DRUKCOMP
#define A_DDBF
#define UpP(x) UPPER(x)
#command INITIALIZE SCREEN => //__run("maz")
#command INIT PRINTER => qqout(eval(memvar->p_init,wasbad))
#define A_XPRN
#define isPrinter .t.
#define A_WAGI
#define DTOV(dat) left(dtoc(dat),5)
#define A_SET_DAT GERMAN
#define STANY     INDX_MAT
#define A_SUMK    -45025516040
#define A_KOMU_N  "Dom Pomocy Spoˆecznej w Pog¢rzu"
#define A_KOMU_A  "Pog¢rze k. Skoczowa"
#define A_AUTOR   "A.D. 1995, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. 8524048"
#define A_DIETA   ! alias()$"ZAPOT,SUROWCE"
#define A_FILELIMIT '48'
#define A_NOZAP
#define DatE() MEMVAR->dzisiaj
#define A_LPNUM 2
#define A_MYSZ
#define PROC_EN memvar->proc_en
#define A_WO_JAD '  3'
#define A_WADO eval(memvar->podpis)
#define ZAP_BIEZ MEMVAR->zap_biez
#define A_MAGSORT
