        interface
          subroutine rigflu(modele,time,nomcmp,tps,nbchar,char,mate,&
     &solvez,ma,nu)
            character(len=8) :: modele
            character(len=24) :: time
            character(len=8) :: nomcmp(6)
            real(kind=8) :: tps(6)
            integer :: nbchar
            character(len=8) :: char
            character(*) :: mate
            character(*) :: solvez
            character(len=8) :: ma
            character(len=14) :: nu
          end subroutine rigflu
        end interface
