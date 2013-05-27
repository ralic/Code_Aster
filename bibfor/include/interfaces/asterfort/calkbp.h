        interface
          subroutine calkbp(nno,ndim,w,dff1,kbp)
            integer :: ndim
            integer :: nno
            real(kind=8) :: w
            real(kind=8) :: dff1(nno,ndim)
            real(kind=8) :: kbp(ndim,nno)
          end subroutine calkbp
        end interface
