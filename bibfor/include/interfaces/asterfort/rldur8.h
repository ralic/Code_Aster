        interface
          subroutine rldur8(nommat,hcol,adia,ablo,neq,nbbloc,xsol,&
     &nbsol)
            integer :: neq
            character(*) :: nommat
            integer :: hcol(*)
            integer :: adia(*)
            integer :: ablo(*)
            integer :: nbbloc
            real(kind=8) :: xsol(neq,*)
            integer :: nbsol
          end subroutine rldur8
        end interface
