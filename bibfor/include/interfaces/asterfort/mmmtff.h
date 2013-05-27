        interface
          subroutine mmmtff(phasep,ndim,nbcps,nnl,wpg,ffl,jacobi,tau1,&
     &tau2,rese,nrese,lambda,coefaf,coefff,matrff)
            character(len=9) :: phasep
            integer :: ndim
            integer :: nbcps
            integer :: nnl
            real(kind=8) :: wpg
            real(kind=8) :: ffl(9)
            real(kind=8) :: jacobi
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: rese(3)
            real(kind=8) :: nrese
            real(kind=8) :: lambda
            real(kind=8) :: coefaf
            real(kind=8) :: coefff
            real(kind=8) :: matrff(18,18)
          end subroutine mmmtff
        end interface
