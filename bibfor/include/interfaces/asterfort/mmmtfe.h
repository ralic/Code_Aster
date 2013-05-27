        interface
          subroutine mmmtfe(phasep,ndim,nne,nnl,nbcps,wpg,jacobi,ffe,&
     &ffl,tau1,tau2,mprojt,rese,nrese,lambda,coefff,matrfe)
            character(len=9) :: phasep
            integer :: ndim
            integer :: nne
            integer :: nnl
            integer :: nbcps
            real(kind=8) :: wpg
            real(kind=8) :: jacobi
            real(kind=8) :: ffe(9)
            real(kind=8) :: ffl(9)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: rese(3)
            real(kind=8) :: nrese
            real(kind=8) :: lambda
            real(kind=8) :: coefff
            real(kind=8) :: matrfe(18,27)
          end subroutine mmmtfe
        end interface
