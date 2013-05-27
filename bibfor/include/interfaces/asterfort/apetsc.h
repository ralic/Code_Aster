        interface
          subroutine apetsc(action,solvez,matasz,rsolu,vcinez,nbsol,&
     &istop,iret)
            character(*) :: action
            character(*) :: solvez
            character(*) :: matasz
            real(kind=8) :: rsolu(*)
            character(*) :: vcinez
            integer :: nbsol
            integer :: istop
            integer :: iret
          end subroutine apetsc
        end interface
