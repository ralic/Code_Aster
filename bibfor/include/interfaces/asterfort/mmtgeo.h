        interface
          subroutine mmtgeo(phasep,ndim,nne,nnm,mprojn,mprojt,wpg,ffe,&
     &ffm,dffm,jacobi,coefac,coefaf,coefff,rese,nrese,lambda,jeu,dlagrc,&
     &mprt1n,mprt2n,gene11,gene21,matree,matrmm,matrem,matrme)
            character(len=9) :: phasep
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
            real(kind=8) :: jeu
            real(kind=8) :: dlagrc
            real(kind=8) :: mprt1n(3,3)
            real(kind=8) :: mprt2n(3,3)
            real(kind=8) :: gene11(3,3)
            real(kind=8) :: gene21(3,3)
            real(kind=8) :: matree(27,27)
            real(kind=8) :: matrmm(27,27)
            real(kind=8) :: matrem(27,27)
            real(kind=8) :: matrme(27,27)
          end subroutine mmtgeo
        end interface
