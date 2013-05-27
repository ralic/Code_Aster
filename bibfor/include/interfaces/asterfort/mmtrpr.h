        interface
          subroutine mmtrpr(ndim,lpenaf,djeut,dlagrf,coefaf,tau1,tau2,&
     &ladhe,rese,nrese)
            integer :: ndim
            logical :: lpenaf
            real(kind=8) :: djeut(3)
            real(kind=8) :: dlagrf(2)
            real(kind=8) :: coefaf
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            logical :: ladhe
            real(kind=8) :: rese(3)
            real(kind=8) :: nrese
          end subroutine mmtrpr
        end interface
