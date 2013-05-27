        interface
          subroutine ttprsm(ndim,ddeple,ddeplm,dlagrf,coeffr,tau1,tau2&
     &,mprojt,inadh,rese,nrese,coeffp,lpenaf,dvitet)
            integer :: ndim
            real(kind=8) :: ddeple(3)
            real(kind=8) :: ddeplm(3)
            real(kind=8) :: dlagrf(2)
            real(kind=8) :: coeffr
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mprojt(3,3)
            integer :: inadh
            real(kind=8) :: rese(3)
            real(kind=8) :: nrese
            real(kind=8) :: coeffp
            logical :: lpenaf
            real(kind=8) :: dvitet(3)
          end subroutine ttprsm
        end interface
