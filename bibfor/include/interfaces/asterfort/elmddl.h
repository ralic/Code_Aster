        interface
          subroutine elmddl(raide,option,neq,ddl,nddle,nbddl,vecddl)
            integer :: nddle
            integer :: neq
            character(len=19) :: raide
            character(len=14) :: option
            character(len=8) :: ddl(nddle)
            integer :: nbddl
            integer :: vecddl(neq)
          end subroutine elmddl
        end interface
