        interface
          subroutine fointe(codmes,nomf,nbpu,nompu,valpu,resu,ier)
            character(*) :: codmes
            character(*) :: nomf
            integer :: nbpu
            character(*) :: nompu(*)
            real(kind=8) :: valpu(*)
            real(kind=8) :: resu
            integer :: ier
          end subroutine fointe
        end interface
