        interface
          subroutine edgini(itemax,prec,pm,eqsitr,mu,gamma,m,n,dp,iret&
     &)
            integer :: itemax
            real(kind=8) :: prec
            real(kind=8) :: pm
            real(kind=8) :: eqsitr
            real(kind=8) :: mu
            real(kind=8) :: gamma(3)
            real(kind=8) :: m(3)
            real(kind=8) :: n(3)
            real(kind=8) :: dp
            integer :: iret
          end subroutine edgini
        end interface
