        interface
          subroutine mmtape(phasep,leltf,ndim,nnl,nne,nnm,nbcps,wpg,&
     &jacobi,ffl,ffe,ffm,norm,tau1,tau2,mprojt,rese,nrese,lambda,coefff,&
     &coefaf,coefac,matrcc,matrff,matrce,matrcm,matrfe,matrfm)
            character(len=9) :: phasep
            logical :: leltf
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
            real(kind=8) :: norm(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: rese(3)
            real(kind=8) :: nrese
            real(kind=8) :: lambda
            real(kind=8) :: coefff
            real(kind=8) :: coefaf
            real(kind=8) :: coefac
            real(kind=8) :: matrcc(9,9)
            real(kind=8) :: matrff(18,18)
            real(kind=8) :: matrce(9,27)
            real(kind=8) :: matrcm(9,27)
            real(kind=8) :: matrfe(18,27)
            real(kind=8) :: matrfm(18,27)
          end subroutine mmtape
        end interface
