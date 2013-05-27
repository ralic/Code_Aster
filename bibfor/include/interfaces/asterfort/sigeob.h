        interface
          subroutine sigeob(eps,bt,endo,ndim,lambda,mu,sigm)
            real(kind=8) :: eps(6)
            real(kind=8) :: bt(6)
            real(kind=8) :: endo
            integer :: ndim
            real(kind=8) :: lambda
            real(kind=8) :: mu
            real(kind=8) :: sigm(6)
          end subroutine sigeob
        end interface
