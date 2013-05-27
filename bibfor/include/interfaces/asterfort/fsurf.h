        interface
          subroutine fsurf(option,nomte,xi,nb1,vecl,vectpt)
            character(len=16) :: option
            character(len=16) :: nomte
            real(kind=8) :: xi(3,*)
            integer :: nb1
            real(kind=8) :: vecl(51)
            real(kind=8) :: vectpt(9,3,3)
          end subroutine fsurf
        end interface
