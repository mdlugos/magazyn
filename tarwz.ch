#include "lan.ch"
#command init screen => __run("font852.com")
#command INIT PRINTER => qqout("@")
#define A_MSCPI
#define STANY INDX_MAT
#define A_15CALI
#define A_STYLUS
#define isPrinter .T.
#define DTOV(dat) tran(subs(dtos(dat),5),"@R XX.XX")
#define A_DRUKCOMP 7
#define A_DLINK
#define A_SET_DAT   GERMAN
#define A_SUMK     -42775790866
#define A_KOMU_N  "Tartak i Stolarnia Kobi¢r"
#define A_KOMU_A  "Kobi¢r, ul. Centralna 56"
#define A_AUTOR   "A.D. 1996, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)524048"
#define A_FILELIMIT '41'
#define A_NAZBEG 7
#define A_NRLTH 5
#define INDEXPIC "####-###-###"
#define A_NOZAP
#define A_KHSEP
#define A_SWWKTM
#define A_FFULL
#define A_AF
#define A_REGON
#define A_IZ
#define A_ZLEC11
#define A_JMTOT
//#define A_JMALTTOT(nz,lam,x) if((lam)->przel#0,(lam)->przel,(x:=asize(getlines(if((lam)->info="NTP",nz,(lam)->info),";"),2),if(x[2]=NIL,x[2]:='1',),round(val(x[1])*val(x[2]),2)))
#define A_FDO
