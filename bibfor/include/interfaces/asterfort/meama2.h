        interface
          subroutine meama2(modele,nchar,lchar,mate,matel,prefch)
            character(len=8) :: modele
            integer :: nchar
            character(len=8) :: lchar(*)
            character(*) :: mate
            character(len=19) :: matel
            character(len=19) :: prefch
          end subroutine meama2
        end interface
