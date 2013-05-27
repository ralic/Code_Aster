        interface
          subroutine mmmtcm(phasep,ndim,nnl,nnm,norm,wpg,ffl,ffm,&
     &jacobi,matrcm)
            character(len=9) :: phasep
            integer :: ndim
            integer :: nnl
            integer :: nnm
            real(kind=8) :: norm(3)
            real(kind=8) :: wpg
            real(kind=8) :: ffl(9)
            real(kind=8) :: ffm(9)
            real(kind=8) :: jacobi
            real(kind=8) :: matrcm(9,27)
          end subroutine mmmtcm
        end interface
