        interface
          subroutine cabhvf(maxfa,maxdim,ndim,nno,nnos,nface,axi,geom,&
     &vol,mface,dface,xface,normfa,uticer)
            integer :: nno
            integer :: ndim
            integer :: maxdim
            integer :: maxfa
            integer :: nnos
            integer :: nface
            logical :: axi
            real(kind=8) :: geom(1:ndim,1:nno)
            real(kind=8) :: vol
            real(kind=8) :: mface(1:maxfa)
            real(kind=8) :: dface(1:maxfa)
            real(kind=8) :: xface(1:maxdim,1:maxfa)
            real(kind=8) :: normfa(1:maxdim,1:maxfa)
            logical :: uticer
          end subroutine cabhvf
        end interface
