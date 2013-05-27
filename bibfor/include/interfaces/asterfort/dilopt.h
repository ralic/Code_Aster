        interface
          subroutine dilopt(dimdef,dimuel,poids,poids2,b,drde,matuu)
            integer :: dimuel
            integer :: dimdef
            real(kind=8) :: poids
            real(kind=8) :: poids2
            real(kind=8) :: b(dimdef,dimuel)
            real(kind=8) :: drde(dimdef,dimdef)
            real(kind=8) :: matuu(dimuel*dimuel)
          end subroutine dilopt
        end interface
