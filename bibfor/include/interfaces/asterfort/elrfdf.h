        interface
          subroutine elrfdf(elrefz,x,dimd,dff,nno,ndim)
            character(*) :: elrefz
            real(kind=8) :: x(*)
            integer :: dimd
            real(kind=8) :: dff(3,*)
            integer :: nno
            integer :: ndim
          end subroutine elrfdf
        end interface
