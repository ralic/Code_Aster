        interface
          subroutine ceobfb(bm,epsm,lambda,mu,ecrob,bdim,fb,nofbm,fbm)
            real(kind=8) :: bm(6)
            real(kind=8) :: epsm(6)
            real(kind=8) :: lambda
            real(kind=8) :: mu
            real(kind=8) :: ecrob
            integer :: bdim
            real(kind=8) :: fb(6)
            real(kind=8) :: nofbm
            real(kind=8) :: fbm(6)
          end subroutine ceobfb
        end interface
