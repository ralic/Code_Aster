        interface
          subroutine vpordo(type,iordre,nbpro,valpro,vecpro,neq)
            integer :: neq
            integer :: nbpro
            integer :: type
            integer :: iordre
            real(kind=8) :: valpro(nbpro)
            real(kind=8) :: vecpro(neq,nbpro)
          end subroutine vpordo
        end interface
