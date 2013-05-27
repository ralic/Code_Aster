        interface
          subroutine meamme(optioz,modele,nchar,lchar,mate,cara,exitim&
     &,time,base,merigi,memass,meamor)
            character(*) :: optioz
            character(*) :: modele
            integer :: nchar
            character(len=8) :: lchar(*)
            character(*) :: mate
            character(*) :: cara
            logical :: exitim
            real(kind=8) :: time
            character(len=1) :: base
            character(*) :: merigi
            character(*) :: memass
            character(*) :: meamor
          end subroutine meamme
        end interface
