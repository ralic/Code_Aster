        interface
          subroutine memame(option,modele,nchar,lchar,mate,carele,&
     &exitim,instan,compor,matelz,base)
            character(*) :: option
            character(*) :: modele
            integer :: nchar
            character(len=8) :: lchar(*)
            character(*) :: mate
            character(*) :: carele
            logical :: exitim
            real(kind=8) :: instan
            character(*) :: compor
            character(*) :: matelz
            character(len=1) :: base
          end subroutine memame
        end interface
