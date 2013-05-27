        interface
          subroutine vectpe(nb1,nb2,vecu,vectn,vecnph,vecpe)
            integer :: nb1
            integer :: nb2
            real(kind=8) :: vecu(8,3)
            real(kind=8) :: vectn(9,3)
            real(kind=8) :: vecnph(9,3)
            real(kind=8) :: vecpe(51)
          end subroutine vectpe
        end interface
