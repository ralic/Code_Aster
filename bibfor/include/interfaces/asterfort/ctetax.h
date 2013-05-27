        interface
          subroutine ctetax(basmod,numa,nbsec,teta,nbtet)
            integer :: nbtet
            character(len=8) :: basmod
            integer :: numa
            integer :: nbsec
            real(kind=8) :: teta(nbtet,nbtet)
          end subroutine ctetax
        end interface
