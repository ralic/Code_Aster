        interface
          subroutine mefger(ndim,som,xint,yint,rint,sgn,orig,beta)
            integer :: ndim(14)
            real(kind=8) :: som(9)
            real(kind=8) :: xint(*)
            real(kind=8) :: yint(*)
            real(kind=8) :: rint(*)
            integer :: sgn(*)
            integer :: orig(*)
            real(kind=8) :: beta(*)
          end subroutine mefger
        end interface
