        interface
          subroutine mmmsta(ndim,leltf,lpenaf,loptf,djeut,dlagrf,&
     &coefaf,coefff,tau1,tau2,lcont,ladhe,lambda,rese,nrese)
            integer :: ndim
            logical :: leltf
            logical :: lpenaf
            logical :: loptf
            real(kind=8) :: djeut(3)
            real(kind=8) :: dlagrf(2)
            real(kind=8) :: coefaf
            real(kind=8) :: coefff
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            logical :: lcont
            logical :: ladhe
            real(kind=8) :: lambda
            real(kind=8) :: rese(3)
            real(kind=8) :: nrese
          end subroutine mmmsta
        end interface
