        interface
          subroutine rlduc8(nommat,hcol,adia,ablo,neq,nbbloc,xsol,&
     &nbsol)
            integer :: neq
            character(*) :: nommat
            integer :: hcol(*)
            integer :: adia(*)
            integer :: ablo(*)
            integer :: nbbloc
            complex(kind=8) :: xsol(neq,*)
            integer :: nbsol
          end subroutine rlduc8
        end interface
