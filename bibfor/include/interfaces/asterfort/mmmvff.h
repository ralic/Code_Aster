        interface
          subroutine mmmvff(phasep,ndim,nnl,nbcps,wpg,ffl,tau1,tau2,&
     &jacobi,coefaf,dlagrf,rese,lambda,coefff,dvite,mprojt,vectff)
            character(len=9) :: phasep
            integer :: ndim
            integer :: nnl
            integer :: nbcps
            real(kind=8) :: wpg
            real(kind=8) :: ffl(9)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: jacobi
            real(kind=8) :: coefaf
            real(kind=8) :: dlagrf(2)
            real(kind=8) :: rese(3)
            real(kind=8) :: lambda
            real(kind=8) :: coefff
            real(kind=8) :: dvite(3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: vectff(18)
          end subroutine mmmvff
        end interface
