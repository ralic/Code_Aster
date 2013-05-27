        interface
          subroutine mmmtfm(phasep,ndim,nnm,nnl,nbcps,wpg,jacobi,ffm,&
     &ffl,tau1,tau2,mprojt,rese,nrese,lambda,coefff,matrfm)
            character(len=9) :: phasep
            integer :: ndim
            integer :: nnm
            integer :: nnl
            integer :: nbcps
            real(kind=8) :: wpg
            real(kind=8) :: jacobi
            real(kind=8) :: ffm(9)
            real(kind=8) :: ffl(9)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: rese(3)
            real(kind=8) :: nrese
            real(kind=8) :: lambda
            real(kind=8) :: coefff
            real(kind=8) :: matrfm(18,27)
          end subroutine mmmtfm
        end interface
