        interface
          subroutine nmactp(sdimpr,sddisc,sderro,defico,resoco,solveu,&
     &parcri,nbiter,numins)
            character(len=24) :: sdimpr
            character(len=19) :: sddisc
            character(len=24) :: sderro
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: solveu
            real(kind=8) :: parcri(*)
            integer :: nbiter
            integer :: numins
          end subroutine nmactp
        end interface
