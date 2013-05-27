        interface
          subroutine invjax(stop,nno,ndim,nderiv,dff,coor,invjac,ipb)
            integer :: ndim
            integer :: nno
            character(len=1) :: stop
            integer :: nderiv
            real(kind=8) :: dff(3,nno)
            real(kind=8) :: coor(ndim*nno)
            real(kind=8) :: invjac(3,3)
            integer :: ipb
          end subroutine invjax
        end interface
