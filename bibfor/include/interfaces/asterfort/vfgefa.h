        interface
          subroutine vfgefa(maxdim,ndim,nbnos,xs,t,xg,surf,norm,xgf,d,&
     &iret)
            integer :: nbnos
            integer :: ndim
            integer :: maxdim
            real(kind=8) :: xs(maxdim,nbnos)
            real(kind=8) :: t(maxdim,nbnos)
            real(kind=8) :: xg(ndim)
            real(kind=8) :: surf
            real(kind=8) :: norm(maxdim)
            real(kind=8) :: xgf(maxdim)
            real(kind=8) :: d
            integer :: iret
          end subroutine vfgefa
        end interface
