        interface
          subroutine elrfvf(elrefz,x,dimf,ff,nno)
            character(*) :: elrefz
            real(kind=8) :: x(*)
            integer :: dimf
            real(kind=8) :: ff(*)
            integer :: nno
          end subroutine elrfvf
        end interface
