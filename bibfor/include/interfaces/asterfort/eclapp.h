        interface
          subroutine eclapp(ndim,nno2,lonmin,coor)
            integer :: nno2
            integer :: ndim
            real(kind=8) :: lonmin
            real(kind=8) :: coor(ndim,nno2)
          end subroutine eclapp
        end interface
