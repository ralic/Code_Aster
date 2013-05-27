        interface
          subroutine dxsiro(ne,t2ve,tensav,tensap)
            integer :: ne
            real(kind=8) :: t2ve(2,2)
            real(kind=8) :: tensav(*)
            real(kind=8) :: tensap(*)
          end subroutine dxsiro
        end interface
