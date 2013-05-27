        interface
          subroutine mltacf(n,ncol,adper,matper,matfi,local,p)
            integer :: n
            integer :: ncol
            integer :: adper(*)
            complex(kind=8) :: matper(*)
            complex(kind=8) :: matfi(*)
            integer(kind=4) :: local(*)
            integer :: p
          end subroutine mltacf
        end interface
