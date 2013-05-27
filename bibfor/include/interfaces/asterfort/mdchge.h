        interface
          subroutine mdchge(numddl,typnum,imode,iamor,pulsat,masgen,&
     &amogen,lflu,nbnli,noecho,logcho,parcho,intitu,ddlcho,ier)
            integer :: nbnli
            character(len=14) :: numddl
            character(len=16) :: typnum
            integer :: imode
            integer :: iamor
            real(kind=8) :: pulsat(*)
            real(kind=8) :: masgen(*)
            real(kind=8) :: amogen(*)
            logical :: lflu
            character(len=8) :: noecho(nbnli,*)
            integer :: logcho(nbnli,*)
            real(kind=8) :: parcho(nbnli,*)
            character(len=8) :: intitu(*)
            integer :: ddlcho(*)
            integer :: ier
          end subroutine mdchge
        end interface
