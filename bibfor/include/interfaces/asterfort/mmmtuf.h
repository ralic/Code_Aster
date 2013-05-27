        interface
          subroutine mmmtuf(phasep,ndim,nnl,nne,nnm,nbcps,wpg,jacobi,&
     &ffl,ffe,ffm,tau1,tau2,mprojt,rese,nrese,lambda,coefff,matref,&
     &matrmf)
            character(len=9) :: phasep
            integer :: ndim
            integer :: nnl
            integer :: nne
            integer :: nnm
            integer :: nbcps
            real(kind=8) :: wpg
            real(kind=8) :: jacobi
            real(kind=8) :: ffl(9)
            real(kind=8) :: ffe(9)
            real(kind=8) :: ffm(9)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: rese(3)
            real(kind=8) :: nrese
            real(kind=8) :: lambda
            real(kind=8) :: coefff
            real(kind=8) :: matref(27,18)
            real(kind=8) :: matrmf(27,18)
          end subroutine mmmtuf
        end interface
