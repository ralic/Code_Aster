        interface
          subroutine mmvape(phasep,leltf,ndim,nnl,nbcps,coefac,coefaf,&
     &coefff,ffl,wpg,jeu,jacobi,lambda,tau1,tau2,mprojt,dlagrc,dlagrf,&
     &dvite,rese,vectcc,vectff)
            character(len=9) :: phasep
            logical :: leltf
            integer :: ndim
            integer :: nnl
            integer :: nbcps
            real(kind=8) :: coefac
            real(kind=8) :: coefaf
            real(kind=8) :: coefff
            real(kind=8) :: ffl(9)
            real(kind=8) :: wpg
            real(kind=8) :: jeu
            real(kind=8) :: jacobi
            real(kind=8) :: lambda
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: dlagrc
            real(kind=8) :: dlagrf(2)
            real(kind=8) :: dvite(3)
            real(kind=8) :: rese(3)
            real(kind=8) :: vectcc(9)
            real(kind=8) :: vectff(18)
          end subroutine mmvape
        end interface
