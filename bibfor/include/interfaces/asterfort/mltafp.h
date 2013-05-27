        interface
          subroutine mltafp(n,ncol,adper,matper,matfi,local)
            integer :: n
            integer :: ncol
            integer :: adper(*)
            real(kind=8) :: matper(*)
            real(kind=8) :: matfi(*)
            integer(kind=4) :: local(*)
          end subroutine mltafp
        end interface
