        interface
          subroutine dfdmip(ndim,nno,axi,geom,g,iw,vff,idfde,r,w,dfdi)
            integer :: nno
            integer :: ndim
            logical :: axi
            real(kind=8) :: geom(ndim,nno)
            integer :: g
            integer :: iw
            real(kind=8) :: vff(nno)
            integer :: idfde
            real(kind=8) :: r
            real(kind=8) :: w
            real(kind=8) :: dfdi(nno,ndim)
          end subroutine dfdmip
        end interface
