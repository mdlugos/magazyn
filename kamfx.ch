#include "lan.ch"
#command INIT SCREEN => __run("font.com")
#command INIT PRINTER => qqout("@P")
#define isPrinter() (.t.)
//#define A_PCL
#define A_ZLEC11
#define A_15CALI
#define A_KASA
#define A_NOPV
#define A_WA
#define A_FIFO
#define DTOV(dat) right(dtoc(dat),5)
#define A_SET_DAT  ANSI
#define STANY indx_mat
#define A_SWW
#define A_SUMK -32698001182
#define A_KOMU_N  'P.P.H. "KAMIX"'
#define A_KOMU_A  "Tychy, ul. Przemys’owa 60"
#define A_AUTOR   "A.D. 1995, Marek D’ugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)524048"
#define A_FILELIMIT '36'
#define A_FA
#define A_FAT
#define A_VAT
#define A_NAZBEG 7
#define A_NRLTH 5
#define INDEXPIC "XXXXXX"
#define A_LFIRST
#define ILEVAT(w,pv) ROUND(pv*.01*w,A_ZAOKR)
#define WbezVAT(w,pv) w
#define WzVAT(w,pv) ROUND((1+pv*.01)*w,A_ZAOKR)
#define W(w,pv) ROUND(w,A_ZAOKR)
#define WPZ(w) W(w,0)
#define VATPZ(w,pv) ILEVAT(w,pv)
#define A_DRUKCOMP
#define A_WTYM
#define A_MYSZ
#define A_FFULL
#define A_FDO
#define A_AUTOMAR
#define cenA_zaK cena_przy
#define A_IZ 9
#define A_ALTCEN
