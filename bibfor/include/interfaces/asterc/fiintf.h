        interface
          subroutine fiintf(nomf,nbpu,nompu,valpu,iret,msgerr,resu)
            character(*) :: nomf
            integer :: nbpu
            character(*) :: nompu
            real(kind=8) :: valpu(*)
            integer :: iret
            character(*) :: msgerr
            real(kind=8) :: resu(*)
          end subroutine fiintf
        end interface
