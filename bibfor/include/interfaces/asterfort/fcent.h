        interface
          subroutine fcent(nomte,xi,nb1,vecl)
            character(len=16) :: nomte
            real(kind=8) :: xi(3,*)
            integer :: nb1
            real(kind=8) :: vecl(51)
          end subroutine fcent
        end interface
