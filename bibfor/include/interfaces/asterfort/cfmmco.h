        interface
          subroutine cfmmco(defico,resoco,izone,nomcoz,action,valr)
            character(len=24) :: defico
            character(len=24) :: resoco
            integer :: izone
            character(*) :: nomcoz
            character(len=1) :: action
            real(kind=8) :: valr
          end subroutine cfmmco
        end interface
