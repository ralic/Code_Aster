        interface
          subroutine rlbfr8(nommat,neq,xsol,nbsm,typsym)
            integer :: neq
            character(*) :: nommat
            real(kind=8) :: xsol(neq,*)
            integer :: nbsm
            integer :: typsym
          end subroutine rlbfr8
        end interface
