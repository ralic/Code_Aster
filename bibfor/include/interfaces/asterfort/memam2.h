        interface
          subroutine memam2(option,modele,nchar,lchar,mate,cara,compor&
     &,exitim,time,chacce,vecel,basez,ligrez)
            character(*) :: option
            character(*) :: modele
            integer :: nchar
            character(len=8) :: lchar(*)
            character(*) :: mate
            character(*) :: cara
            character(len=24) :: compor
            logical :: exitim
            real(kind=8) :: time
            character(*) :: chacce
            character(*) :: vecel
            character(*) :: basez
            character(*) :: ligrez
          end subroutine memam2
        end interface
