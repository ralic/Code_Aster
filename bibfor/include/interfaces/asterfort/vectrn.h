        interface
          subroutine vectrn(nb2,vectpt,vectn,vecthe,vecnph,blam)
            integer :: nb2
            real(kind=8) :: vectpt(9,2,3)
            real(kind=8) :: vectn(9,3)
            real(kind=8) :: vecthe(9,3)
            real(kind=8) :: vecnph(9,3)
            real(kind=8) :: blam(9,3,3)
          end subroutine vectrn
        end interface
