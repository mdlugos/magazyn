#include "lan.ch"
#define isPrinter .t.
#define A_XPRN
#define A_PCL
#command init screen  => //__run("font.com")
#command INIT PRINTER => qqout(memvar->p_init)
/*                                ;
  x:=SET(17,.f.)                                         ;
; if wasbad .and. isprinter()                            ;
;   QQOUT(MEMOREAD("epson.fnt"))                         ;
;   wasbad:=.f.                                          ;
; end                                                    ;
; qqout("@P%1")                                      ;
; set(17,x)
*/
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define A_DRUKCOMP
#define A_WA
#define A_WE
#define warT_ewiD wartosc
#define A_SET_DAT   GERMAN
//#define A_OLDA
#define STANY   INDX_MAT
//#define A_OLZA
#define A_SUMK    -40627662684
#define A_KOMU_N  'KRAFT FOODS oddziaà Olza Sp. z o.o.'
#define A_KOMU_A  "Cieszyn, Maàa ù•ka"
#define A_AUTOR   "A.D. 1996, Marek Dàugosz, Cieszyn, ul. Korfantego 24, tel. 8524048"
#define A_FILELIMIT '37'
#define A_NAZBEG 7
#define A_NRLTH 5
//#define A_ZAM
#define INDEXPIC "XXX-###XXXXX"
#define A_LFIRST
#define A_SWWKTM
#define A_DLINK
#define A_NOZAP
#define A_MYSZ
#define A_ZLEC11
