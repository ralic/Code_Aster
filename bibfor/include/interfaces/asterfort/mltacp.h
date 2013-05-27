        interface
          subroutine mltacp(n,ncol,adper,matper,matfi,local)
            integer :: n
            integer :: ncol
            integer :: adper(*)
            complex(kind=8) :: matper(*)
            complex(kind=8) :: matfi(*)
            integer(kind=4) :: local(*)
          end subroutine mltacp
        end interface
