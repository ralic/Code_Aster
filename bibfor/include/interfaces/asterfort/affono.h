        interface
          subroutine affono(valr,valk,desc,prnm,nbcomp,fonree,nomn,ino&
     &,nsurch,forimp,valfor,valfof,motcle,verif,nbec)
            integer :: nbcomp
            real(kind=8) :: valr(1)
            character(len=8) :: valk(1)
            integer :: desc
            integer :: prnm(1)
            character(len=4) :: fonree
            character(len=8) :: nomn
            integer :: ino
            integer :: nsurch
            integer :: forimp(nbcomp)
            real(kind=8) :: valfor(nbcomp)
            character(len=8) :: valfof(nbcomp)
            character(len=16) :: motcle(nbcomp)
            logical :: verif
            integer :: nbec
          end subroutine affono
        end interface
