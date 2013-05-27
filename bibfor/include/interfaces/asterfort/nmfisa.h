        interface
          subroutine nmfisa(axi,geom,kpg,poids,b)
            logical :: axi
            real(kind=8) :: geom(2,4)
            integer :: kpg
            real(kind=8) :: poids
            real(kind=8) :: b(2,8)
          end subroutine nmfisa
        end interface
