        interface
          subroutine mdgep4(neq,nbexci,psidel,temps,nomfon,iddl,rep)
            integer :: neq
            integer :: nbexci
            real(kind=8) :: psidel(neq,*)
            real(kind=8) :: temps
            character(len=8) :: nomfon(*)
            integer :: iddl
            real(kind=8) :: rep
          end subroutine mdgep4
        end interface
