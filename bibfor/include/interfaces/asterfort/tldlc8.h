        interface
          subroutine tldlc8(nommat,hcol,adia,ablo,npivot,neq,nbbloc,&
     &ildeb,ilfin,eps)
            character(*) :: nommat
            integer :: hcol(*)
            integer :: adia(*)
            integer :: ablo(*)
            integer :: npivot
            integer :: neq
            integer :: nbbloc
            integer :: ildeb
            integer :: ilfin
            real(kind=8) :: eps
          end subroutine tldlc8
        end interface
