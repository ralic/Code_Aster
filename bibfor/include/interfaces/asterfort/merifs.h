        interface
          subroutine merifs(modele,nchar,lchar,mate,cara,exitim,time,&
     &matel,nh)
            character(len=8) :: modele
            integer :: nchar
            character(*) :: lchar(*)
            character(*) :: mate
            character(len=8) :: cara
            logical :: exitim
            real(kind=8) :: time
            character(len=19) :: matel
            integer :: nh
          end subroutine merifs
        end interface
