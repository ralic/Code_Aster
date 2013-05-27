        interface
          subroutine trmult(modsta,numexi,mailla,neq,iddeeq,pside)
            integer :: neq
            character(len=8) :: modsta
            integer :: numexi
            character(len=8) :: mailla
            integer :: iddeeq
            real(kind=8) :: pside(neq)
          end subroutine trmult
        end interface
