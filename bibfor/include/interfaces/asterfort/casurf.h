        interface
          subroutine casurf(ndim,nno,geom,surff)
            integer :: nno
            integer :: ndim
            real(kind=8) :: geom(ndim,nno)
            real(kind=8) :: surff
          end subroutine casurf
        end interface
