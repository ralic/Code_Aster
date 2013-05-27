        interface
          subroutine betcvx(nmat,mater,sig,vind,vinf,elgeom,nvi,nseuil&
     &)
            integer :: nmat
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
            real(kind=8) :: elgeom(*)
            integer :: nvi
            integer :: nseuil
          end subroutine betcvx
        end interface
