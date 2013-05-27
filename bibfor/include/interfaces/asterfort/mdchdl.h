        interface
          subroutine mdchdl(nbnli,noecho,lnoue2,iliai,ddlcho,ier)
            integer :: nbnli
            character(len=8) :: noecho(nbnli,*)
            logical :: lnoue2
            integer :: iliai
            integer :: ddlcho(*)
            integer :: ier
          end subroutine mdchdl
        end interface
