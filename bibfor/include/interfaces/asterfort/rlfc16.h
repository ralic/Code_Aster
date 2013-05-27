        interface
          subroutine rlfc16(nommat,neq,cxsol,nbsol,typsym)
            integer :: neq
            character(*) :: nommat
            complex(kind=8) :: cxsol(neq,*)
            integer :: nbsol
            integer :: typsym
          end subroutine rlfc16
        end interface
