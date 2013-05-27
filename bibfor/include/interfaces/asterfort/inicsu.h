        interface
          subroutine inicsu(valcen,valcev,valfac,valfav,maxfa)
            integer :: maxfa
            real(kind=8) :: valcen(14,6)
            real(kind=8) :: valcev(14,6,maxfa)
            real(kind=8) :: valfac(maxfa,14,6)
            real(kind=8) :: valfav(maxfa,14,6,maxfa)
          end subroutine inicsu
        end interface
