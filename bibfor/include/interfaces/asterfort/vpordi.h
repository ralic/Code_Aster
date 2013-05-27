        interface
          subroutine vpordi(type,iordre,nbpro,valpro,vecpro,neq,indpro&
     &)
            integer :: neq
            integer :: nbpro
            integer :: type
            integer :: iordre
            real(kind=8) :: valpro(*)
            real(kind=8) :: vecpro(neq,nbpro)
            integer :: indpro(*)
          end subroutine vpordi
        end interface
