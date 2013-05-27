        interface
          subroutine betfpp(materf,nmat,elgeom,pc,pt,nseuil,fc,ft,&
     &dfcdlc,dftdlt,kuc,kut,ke)
            integer :: nmat
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: elgeom(*)
            real(kind=8) :: pc
            real(kind=8) :: pt
            integer :: nseuil
            real(kind=8) :: fc
            real(kind=8) :: ft
            real(kind=8) :: dfcdlc
            real(kind=8) :: dftdlt
            real(kind=8) :: kuc
            real(kind=8) :: kut
            real(kind=8) :: ke
          end subroutine betfpp
        end interface
