        interface
          subroutine afddli(valr,valk,valc,prnm,nddla,fonree,nomn,ino,&
     &ddlimp,valimr,valimf,valimc,motcle,direct,dimens,mod,lisrel,nomcmp&
     &,nbcmp,icompt,lxfem,jnoxfl,jnoxfv,ch1,ch2,ch3,cnxinv)
            integer :: nddla
            real(kind=8) :: valr(*)
            character(len=8) :: valk(*)
            complex(kind=8) :: valc(*)
            integer :: prnm(*)
            character(len=4) :: fonree
            character(len=8) :: nomn
            integer :: ino
            integer :: ddlimp(nddla)
            real(kind=8) :: valimr(nddla)
            character(len=8) :: valimf(nddla)
            complex(kind=8) :: valimc(nddla)
            character(len=16) :: motcle(nddla)
            real(kind=8) :: direct(3)
            integer :: dimens
            character(len=8) :: mod
            character(len=19) :: lisrel
            character(len=8) :: nomcmp(*)
            integer :: nbcmp
            integer :: icompt(nddla)
            logical :: lxfem
            integer :: jnoxfl
            integer :: jnoxfv
            character(len=19) :: ch1
            character(len=19) :: ch2
            character(len=19) :: ch3
            character(len=19) :: cnxinv
          end subroutine afddli
        end interface
