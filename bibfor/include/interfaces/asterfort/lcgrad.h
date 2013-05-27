        interface
          subroutine lcgrad(resi,rigi,ndim,ndimsi,neps,sigma,apg,lag,&
     &grad,aldc,r,c,ktg,sig,dsidep)
            integer :: neps
            integer :: ndim
            logical :: resi
            logical :: rigi
            integer :: ndimsi
            real(kind=8) :: sigma(6)
            real(kind=8) :: apg
            real(kind=8) :: lag
            real(kind=8) :: grad(ndim)
            real(kind=8) :: aldc
            real(kind=8) :: r
            real(kind=8) :: c
            real(kind=8) :: ktg(6,6,4)
            real(kind=8) :: sig(neps)
            real(kind=8) :: dsidep(neps,neps)
          end subroutine lcgrad
        end interface
