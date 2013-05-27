        interface
          subroutine nmelcm(phase,modele,defico,resoco,mate,depmoi,&
     &depdel,vitmoi,vitplu,accmoi,mectce)
            character(len=4) :: phase
            character(len=24) :: modele
            character(len=24) :: defico
            character(len=24) :: resoco
            character(*) :: mate
            character(len=19) :: depmoi
            character(len=19) :: depdel
            character(len=19) :: vitmoi
            character(len=19) :: vitplu
            character(len=19) :: accmoi
            character(len=19) :: mectce
          end subroutine nmelcm
        end interface
