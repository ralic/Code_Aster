        interface
          subroutine vpordc(type,iordre,nbpro,valpro,vecpro,neq)
            integer :: neq
            integer :: nbpro
            integer :: type
            integer :: iordre
            complex(kind=8) :: valpro(*)
            complex(kind=8) :: vecpro(neq,nbpro)
          end subroutine vpordc
        end interface
