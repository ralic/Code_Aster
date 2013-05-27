        interface
          subroutine nmstat(phase,fonact,sdstat,sdtime,sdimpr,defico)
            character(len=1) :: phase
            integer :: fonact(*)
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=24) :: sdimpr
            character(len=24) :: defico
          end subroutine nmstat
        end interface
