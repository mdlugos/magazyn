#include "lan.ch"
#define PC852
#define A_CDX DBFMDX
#define UpP(x) UPPER(x)
#define A_KHSEP
#command INIT PRINTER => qqout(eval(MEMVAR->p_init,wasbad))
#define A_XPRN
#define isPrinter (.t.)
#command init screen =>
#define STANY indx_mat
#define A_FIFO
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define A_WA
#define A_SET_DAT  GERMAN
#define A_DRUKCOMP
#define A_SHORTIND
#define A_TRWALOSC
#define A_DIETA   .t. //alias()#"INDX_MAT"
#define A_FILELIMIT '35'
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "####"
#define A_MYSZ
#define A_DLINK
#define A_LPNUM 3
#define A_WADO
#define A_NOZAP
#define A_SB
#define A_SUMK    -39818066027
#define A_KOMU_N  'Mˆodzie¾owy O˜rodek Wychowawczy'
#define A_KOMU_A  'Jaworze, ul. Paˆacowa 1'
#define A_AUTOR   'A.D. 2001, Marek Dˆugosz, Cieszyn, ul. Korfantego 24, tel. (0-33)8524048'
