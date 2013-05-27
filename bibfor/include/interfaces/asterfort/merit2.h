        interface
          subroutine merit2(modele,nchar,lchar,cara,time,matel,prefch,&
     &numero,base)
            character(len=8) :: modele
            integer :: nchar
            character(*) :: lchar(*)
            character(len=8) :: cara
            character(len=24) :: time
            character(len=19) :: matel
            character(len=19) :: prefch
            integer :: numero
            character(len=1) :: base
          end subroutine merit2
        end interface
