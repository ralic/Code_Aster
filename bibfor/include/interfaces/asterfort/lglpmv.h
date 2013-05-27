        interface
          subroutine lglpmv(cumul,n,a,x,y)
            character(*) :: cumul
            integer :: n
            real(kind=8) :: a(6,6)
            real(kind=8) :: x(6)
            real(kind=8) :: y(6)
          end subroutine lglpmv
        end interface
