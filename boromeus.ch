#include "mdstd.ch"
#define A_DDBF
#define A_CDX DBFCDX
#define PC852
#define PLWIN
#define A_EXT HB_TRANSLATE, HB_CODEPAGE_PLMAZ
#define A_WIN_PRN .t.
#define UpP(x) UPPER(x)
#define A_FIFO
#command INIT SCREEN => //__run("font852")
#define isPrinter (.T.)
#define A_PCL
#define A_XPRN
#define A_15CPI
//#define A_OKI4W
#define A_STULUS
#define A_BACKUP memvar->bejkap
#define A_DRUKCOMP
#define A_DOKCOMP 8
#command INIT PRINTER => specout(eval(MEMVAR->p_init,wasbad))
#define A_GETLPT    eval(memvar->do_getlpt)
#define A_PRINT(x)  eval(memvar->do_print,x)
#define A_WA
#define A_WE
#define A_VAT
#define DTOV(dat) tran(subs(dtos(dat),5),"@R ##.##")
#define A_SET_DAT GERMAN
#define DatE() MEMVAR->dzisiaj
#define A_DLINK
#define A_SHORTIND
#define A_SWWKTM 'BLOZ'
#define STANY INDX_MAT
#define A_SUMK    -37214834679
#define A_KOMU_N  "Zak. Opiek. - Leczn. S.S.Boromeuszek"
#define A_KOMU_A  "Cieszyn, ul. G¢rny Rynek 6"
#define A_AUTOR   "A.D. 1994-2014, Marek Dˆugosz, Cieszyn, ul. R¢wna 16, tel. 338522553"
#define A_TRWALOSC
#define A_FILELIMIT '43'
#define A_DIETA   alias()#"INDX_MAT"
#define A_NAZBEG 5
#define A_NRLTH 3
#define INDEXPIC "####"
#define A_MYSZ
#define A_NOZAP
#define A_KOD ean
#define A_KODY "EAN"
#define KoD ean
#define A_ZAZNACZ
#define A_NOMZ
#define A_ZLEC11
#define A_KODVAL {|g,y,z,x|y:=UpP(trim(y)),!g:changed.or.empty(y).or.isdigit(y).and.len(y)=13.or.(z:=select(),sel('BAZYL',if(isdigit(y),'EAN','NAZWA')),x:=szukam({,,,,if(isdigit(y),1,15),len(y),'Wybierz '+A_KODY,{||KoD+'|'+left(nazwa,40)+'³'+trim(field->dawka)+' '+field->opakowanie},,y}),if(x,kw:=KoD,),dbselectarea(z),x)}
