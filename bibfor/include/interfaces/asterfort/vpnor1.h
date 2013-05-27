        interface
          subroutine vpnor1(norm,neq,nbmode,ddlexc,vecpro,isign,numddl&
     &,coef)
            integer :: neq
            character(*) :: norm
            integer :: nbmode
            integer :: ddlexc(*)
            real(kind=8) :: vecpro(neq,*)
            integer :: isign
            integer :: numddl
            real(kind=8) :: coef(*)
          end subroutine vpnor1
        end interface
