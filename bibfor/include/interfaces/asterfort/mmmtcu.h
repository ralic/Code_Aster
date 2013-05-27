        interface
          subroutine mmmtcu(phasep,ndim,nnl,nne,nnm,norm,wpg,ffl,ffe,&
     &ffm,jacobi,matrce,matrcm)
            character(len=9) :: phasep
            integer :: ndim
            integer :: nnl
            integer :: nne
            integer :: nnm
            real(kind=8) :: norm(3)
            real(kind=8) :: wpg
            real(kind=8) :: ffl(9)
            real(kind=8) :: ffe(9)
            real(kind=8) :: ffm(9)
            real(kind=8) :: jacobi
            real(kind=8) :: matrce(9,27)
            real(kind=8) :: matrcm(9,27)
          end subroutine mmmtcu
        end interface
