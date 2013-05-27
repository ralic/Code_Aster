        interface
          subroutine meriro(modele,cara,nchar,lchar,mate,exitim,time,&
     &compor,matel)
            character(len=8) :: modele
            character(len=8) :: cara
            integer :: nchar
            character(len=8) :: lchar(*)
            character(len=24) :: mate
            logical :: exitim
            real(kind=8) :: time
            character(len=24) :: compor
            character(len=19) :: matel
          end subroutine meriro
        end interface
