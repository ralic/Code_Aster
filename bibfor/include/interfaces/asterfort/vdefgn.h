        interface
          subroutine vdefgn(nomte,nb2,epais,zic,sigma,effgtg)
            character(len=16) :: nomte
            integer :: nb2
            real(kind=8) :: epais
            real(kind=8) :: zic
            real(kind=8) :: sigma(6,*)
            real(kind=8) :: effgtg(8,*)
          end subroutine vdefgn
        end interface
