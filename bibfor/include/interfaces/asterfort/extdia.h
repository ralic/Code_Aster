        interface
          subroutine extdia(matr,numddl,icode,diag)
            character(len=8) :: matr
            character(len=24) :: numddl
            integer :: icode
            real(kind=8) :: diag(*)
          end subroutine extdia
        end interface
