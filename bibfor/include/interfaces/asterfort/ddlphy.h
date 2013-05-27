        interface
          subroutine ddlphy(depplu,neq,vect,desc)
            character(len=19) :: depplu
            integer :: neq
            real(kind=8) :: vect(*)
            character(len=8) :: desc(*)
          end subroutine ddlphy
        end interface
