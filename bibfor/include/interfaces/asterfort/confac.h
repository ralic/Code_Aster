        interface
          subroutine confac(typma,ft,nbft,f,nbf)
            character(len=8) :: typma
            integer :: ft(12,3)
            integer :: nbft
            integer :: f(6,4)
            integer :: nbf
          end subroutine confac
        end interface
