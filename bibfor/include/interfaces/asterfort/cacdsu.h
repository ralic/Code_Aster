        interface
          subroutine cacdsu(maxfa,maxdim,alpha,ndim,nno,nface,geom,vol&
     &,mface,dface,xface,normfa,kdiag,yss,c,d)
            integer :: nno
            integer :: ndim
            integer :: maxdim
            integer :: maxfa
            real(kind=8) :: alpha
            integer :: nface
            real(kind=8) :: geom(ndim,nno)
            real(kind=8) :: vol
            real(kind=8) :: mface(maxfa)
            real(kind=8) :: dface(maxfa)
            real(kind=8) :: xface(maxdim,maxfa)
            real(kind=8) :: normfa(maxdim,maxfa)
            real(kind=8) :: kdiag(6)
            real(kind=8) :: yss(maxdim,maxfa,maxfa)
            real(kind=8) :: c(maxfa,maxfa)
            real(kind=8) :: d(maxfa,maxfa)
          end subroutine cacdsu
        end interface
