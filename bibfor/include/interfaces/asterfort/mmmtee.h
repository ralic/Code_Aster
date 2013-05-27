        interface
          subroutine mmmtee(phasep,ndim,nne,mprojn,mprojt,wpg,ffe,&
     &jacobi,coefac,coefaf,coefff,rese,nrese,lambda,matree)
            character(len=9) :: phasep
            integer :: ndim
            integer :: nne
            real(kind=8) :: mprojn(3,3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: wpg
            real(kind=8) :: ffe(9)
            real(kind=8) :: jacobi
            real(kind=8) :: coefac
            real(kind=8) :: coefaf
            real(kind=8) :: coefff
            real(kind=8) :: rese(3)
            real(kind=8) :: nrese
            real(kind=8) :: lambda
            real(kind=8) :: matree(27,27)
          end subroutine mmmtee
        end interface
