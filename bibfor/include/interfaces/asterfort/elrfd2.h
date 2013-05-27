        interface
          subroutine elrfd2(elrefz,x,dimd,dff2,nno,ndim)
            character(*) :: elrefz
            real(kind=8) :: x(*)
            integer :: dimd
            real(kind=8) :: dff2(3,3,*)
            integer :: nno
            integer :: ndim
          end subroutine elrfd2
        end interface
