        interface
          subroutine rltfr8(nommat,neq,xsol,nbsol,typsym)
            integer :: neq
            character(*) :: nommat
            real(kind=8) :: xsol(neq,*)
            integer :: nbsol
            integer :: typsym
          end subroutine rltfr8
        end interface
