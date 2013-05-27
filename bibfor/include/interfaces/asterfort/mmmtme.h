        interface
          subroutine mmmtme(phasep,lnewtg,ndim,nne,nnm,mprojn,mprojt,&
     &wpg,ffe,ffm,dffm,jacobi,coefac,coefaf,coefff,rese,nrese,lambda,&
     &dlagrc,jeu,h11t1n,h12t2n,h21t1n,h22t2n,matrme)
            character(len=9) :: phasep
            logical :: lnewtg
            integer :: ndim
            integer :: nne
            integer :: nnm
            real(kind=8) :: mprojn(3,3)
            real(kind=8) :: mprojt(3,3)
            real(kind=8) :: wpg
            real(kind=8) :: ffe(9)
            real(kind=8) :: ffm(9)
            real(kind=8) :: dffm(2,9)
            real(kind=8) :: jacobi
            real(kind=8) :: coefac
            real(kind=8) :: coefaf
            real(kind=8) :: coefff
            real(kind=8) :: rese(3)
            real(kind=8) :: nrese
            real(kind=8) :: lambda
            real(kind=8) :: dlagrc
            real(kind=8) :: jeu
            real(kind=8) :: h11t1n(3,3)
            real(kind=8) :: h12t2n(3,3)
            real(kind=8) :: h21t1n(3,3)
            real(kind=8) :: h22t2n(3,3)
            real(kind=8) :: matrme(27,27)
          end subroutine mmmtme
        end interface
