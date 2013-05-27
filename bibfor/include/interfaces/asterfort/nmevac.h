        interface
          subroutine nmevac(sdimpr,sddisc,sderro,defico,resoco,solveu,&
     &ievdac,numins,iterat,retact)
            character(len=24) :: sdimpr
            character(len=19) :: sddisc
            character(len=24) :: sderro
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: solveu
            integer :: ievdac
            integer :: numins
            integer :: iterat
            integer :: retact
          end subroutine nmevac
        end interface
