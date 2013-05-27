        interface
          subroutine lcrotg(indice,dp,e,dtaudf)
            integer :: indice
            real(kind=8) :: dp
            real(kind=8) :: e(6)
            real(kind=8) :: dtaudf(6,3,3)
          end subroutine lcrotg
        end interface
