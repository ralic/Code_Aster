        interface
          subroutine mmmtce(phasep,ndim,nnl,nne,norm,wpg,ffl,ffe,&
     &jacobi,matrce)
            character(len=9) :: phasep
            integer :: ndim
            integer :: nnl
            integer :: nne
            real(kind=8) :: norm(3)
            real(kind=8) :: wpg
            real(kind=8) :: ffl(9)
            real(kind=8) :: ffe(9)
            real(kind=8) :: jacobi
            real(kind=8) :: matrce(9,27)
          end subroutine mmmtce
        end interface
