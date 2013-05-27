        interface
          subroutine mdgep3(neq,nbexci,psidel,temps,nomfon,tab)
            integer :: nbexci
            integer :: neq
            real(kind=8) :: psidel(neq,nbexci)
            real(kind=8) :: temps
            character(len=8) :: nomfon(2*nbexci)
            real(kind=8) :: tab(neq)
          end subroutine mdgep3
        end interface
