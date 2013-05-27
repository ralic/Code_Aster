        interface
          subroutine edgiso(dp,pm,eqsitr,mu,gamma,m,n,seuil,dseuil)
            real(kind=8) :: dp
            real(kind=8) :: pm
            real(kind=8) :: eqsitr
            real(kind=8) :: mu
            real(kind=8) :: gamma(3)
            real(kind=8) :: m(3)
            real(kind=8) :: n(3)
            real(kind=8) :: seuil
            real(kind=8) :: dseuil
          end subroutine edgiso
        end interface
