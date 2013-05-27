        interface
          subroutine teneps(jrwork,adr,sig,eps,epse,epsp)
            integer :: jrwork
            integer :: adr
            real(kind=8) :: sig(6)
            real(kind=8) :: eps(6)
            real(kind=8) :: epse(6)
            real(kind=8) :: epsp(6)
          end subroutine teneps
        end interface
