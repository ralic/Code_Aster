        interface
          subroutine mltaff(n,ncol,adper,matper,matfi,local,p)
            integer :: n
            integer :: ncol
            integer :: adper(*)
            real(kind=8) :: matper(*)
            real(kind=8) :: matfi(*)
            integer(kind=4) :: local(*)
            integer :: p
          end subroutine mltaff
        end interface
