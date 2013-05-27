        interface
          subroutine dilsga(dimdef,dimuel,poids,poids2,b,r,vectu)
            integer :: dimuel
            integer :: dimdef
            real(kind=8) :: poids
            real(kind=8) :: poids2
            real(kind=8) :: b(dimdef,dimuel)
            real(kind=8) :: r(dimdef)
            real(kind=8) :: vectu(dimuel)
          end subroutine dilsga
        end interface
