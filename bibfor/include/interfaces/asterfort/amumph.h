        interface
          subroutine amumph(action,solvez,matasz,rsolu,csolu,vcinez,&
     &nbsol,iret,prepos)
            character(*) :: action
            character(*) :: solvez
            character(*) :: matasz
            real(kind=8) :: rsolu(*)
            complex(kind=8) :: csolu(*)
            character(*) :: vcinez
            integer :: nbsol
            integer :: iret
            logical :: prepos
          end subroutine amumph
        end interface
