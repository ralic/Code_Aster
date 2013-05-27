        interface
          subroutine mminfp(izone,defico,questz,irep,rrep,lrep)
            integer :: izone
            character(len=24) :: defico
            character(*) :: questz
            integer :: irep(*)
            real(kind=8) :: rrep(*)
            logical :: lrep(*)
          end subroutine mminfp
        end interface
