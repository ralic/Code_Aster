        interface
          subroutine prmono(champ,ioc,som,nbcmp,nocmp)
            character(*) :: champ
            integer :: ioc
            real(kind=8) :: som(1)
            integer :: nbcmp
            character(len=8) :: nocmp(1)
          end subroutine prmono
        end interface
