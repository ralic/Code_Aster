        interface
          subroutine mmmtuc(phasep,ndim,nnl,nne,nnm,norm,tau1,tau2,&
     &mprojt,wpg,ffl,ffe,ffm,jacobi,coefff,coefaf,dlagrf,djeut,rese,&
     &nrese,matrec,matrmc)
            character(len=9) :: phasep
            integer :: ndim
            integer :: nnl
            integer :: nne
            integer :: nnm
            real(kind=8) :: norm(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: wpg
            real(kind=8) :: ffl(9)
            real(kind=8) :: ffe(9)
            real(kind=8) :: ffm(9)
            real(kind=8) :: jacobi
            real(kind=8) :: coefff
            real(kind=8) :: coefaf
            real(kind=8) :: dlagrf(2)
            real(kind=8) :: djeut(3)
            real(kind=8) :: rese(3)
            real(kind=8) :: nrese
            real(kind=8) :: matrec(27,9)
            real(kind=8) :: matrmc(27,9)
          end subroutine mmmtuc
        end interface
