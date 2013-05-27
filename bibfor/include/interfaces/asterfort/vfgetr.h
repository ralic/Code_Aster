        interface
          subroutine vfgetr(maxdim,ndim,nbnos,xs,t,xg,surf,norm,xgf,d)
            integer :: nbnos
            integer :: maxdim
            integer :: ndim
            real(kind=8) :: xs(1:maxdim,nbnos)
            real(kind=8) :: t(1:maxdim,2)
            real(kind=8) :: xg(1:maxdim)
            real(kind=8) :: surf
            real(kind=8) :: norm(1:maxdim)
            real(kind=8) :: xgf(1:maxdim)
            real(kind=8) :: d
          end subroutine vfgetr
        end interface
