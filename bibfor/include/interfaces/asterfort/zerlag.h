        interface
          subroutine zerlag(typc,vectr,vectz,nbddl,ideeq)
            integer :: nbddl
            character(len=1) :: typc
            real(kind=8) :: vectr(nbddl)
            complex(kind=8) :: vectz(nbddl)
            integer :: ideeq(2,nbddl)
          end subroutine zerlag
        end interface
