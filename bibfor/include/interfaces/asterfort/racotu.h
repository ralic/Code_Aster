        interface
          subroutine racotu(iprno,lonlis,klisno,noepou,noma,ligrel,mod&
     &,cara,numddl,typlag,lisrel,coorig)
            integer :: lonlis
            integer :: iprno(*)
            character(len=8) :: klisno(lonlis)
            character(len=8) :: noepou
            character(len=8) :: noma
            character(len=19) :: ligrel
            character(len=8) :: mod
            character(len=8) :: cara
            character(len=14) :: numddl
            character(len=2) :: typlag
            character(len=19) :: lisrel
            real(kind=8) :: coorig(3)
          end subroutine racotu
        end interface
