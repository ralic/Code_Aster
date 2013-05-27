        interface
          subroutine caurtg(nomte,ncmp,sigmau,sigrtg)
            integer :: ncmp
            character(len=16) :: nomte
            real(kind=8) :: sigmau(ncmp,1)
            real(kind=8) :: sigrtg(ncmp,1)
          end subroutine caurtg
        end interface
