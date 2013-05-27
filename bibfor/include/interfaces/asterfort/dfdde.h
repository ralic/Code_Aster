        interface
          subroutine dfdde(eps,endo,ndim,lambda,mu,dfde)
            real(kind=8) :: eps(6)
            real(kind=8) :: endo
            integer :: ndim
            real(kind=8) :: lambda
            real(kind=8) :: mu
            real(kind=8) :: dfde(6)
          end subroutine dfdde
        end interface
