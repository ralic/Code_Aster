        interface
          subroutine mmtfpe(phasep,iresof,ndim,nne,nnm,nnl,nbcps,wpg,&
     &jacobi,ffl,ffe,ffm,dffm,norm,tau1,tau2,mprojn,mprojt,rese,nrese,&
     &lambda,jeu,coefff,coefaf,coefac,dlagrc,dlagrf,djeut,matree,matrmm,&
     &matrem,matrme,matrec,matrmc,matref,matrmf)
            character(len=9) :: phasep
            integer :: iresof
            integer :: ndim
            integer :: nne
            integer :: nnm
            integer :: nnl
            integer :: nbcps
            real(kind=8) :: wpg
            real(kind=8) :: jacobi
            real(kind=8) :: ffl(9)
            real(kind=8) :: ffe(9)
            real(kind=8) :: ffm(9)
            real(kind=8) :: dffm(2,9)
            real(kind=8) :: norm(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mprojn(3,3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: rese(3)
            real(kind=8) :: nrese
            real(kind=8) :: lambda
            real(kind=8) :: jeu
            real(kind=8) :: coefff
            real(kind=8) :: coefaf
            real(kind=8) :: coefac
            real(kind=8) :: dlagrc
            real(kind=8) :: dlagrf(2)
            real(kind=8) :: djeut(3)
            real(kind=8) :: matree(27,27)
            real(kind=8) :: matrmm(27,27)
            real(kind=8) :: matrem(27,27)
            real(kind=8) :: matrme(27,27)
            real(kind=8) :: matrec(27,9)
            real(kind=8) :: matrmc(27,9)
            real(kind=8) :: matref(27,18)
            real(kind=8) :: matrmf(27,18)
          end subroutine mmtfpe
        end interface
