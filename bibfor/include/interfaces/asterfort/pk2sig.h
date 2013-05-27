        interface
          subroutine pk2sig(ndim,f,jac,pk2,sig,ind)
            integer :: ndim
            real(kind=8) :: f(3,3)
            real(kind=8) :: jac
            real(kind=8) :: pk2(2*ndim)
            real(kind=8) :: sig(2*ndim)
            integer :: ind
          end subroutine pk2sig
        end interface
