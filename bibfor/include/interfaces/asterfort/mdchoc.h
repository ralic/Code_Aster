        interface
          subroutine mdchoc(nbnli,nbchoc,nbflam,nbsism,nbrfis,nbpal,&
     &logcho,dplmod,parcho,noecho,intitu,ps1del,ps2del,numddl,nbmode,&
     &pulsat,masgen,lamor,amogen,bmodal,neq,nexcit,info,lflu,monmot,ier)
            integer :: nexcit
            integer :: neq
            integer :: nbmode
            integer :: nbnli
            integer :: nbchoc
            integer :: nbflam
            integer :: nbsism
            integer :: nbrfis
            integer :: nbpal
            integer :: logcho(nbnli,*)
            real(kind=8) :: dplmod(nbnli,nbmode,*)
            real(kind=8) :: parcho(nbnli,*)
            character(len=8) :: noecho(nbnli,*)
            character(len=8) :: intitu(*)
            real(kind=8) :: ps1del(neq,nexcit)
            real(kind=8) :: ps2del(nbnli,nexcit,*)
            character(len=14) :: numddl
            real(kind=8) :: pulsat(*)
            real(kind=8) :: masgen(*)
            logical :: lamor
            real(kind=8) :: amogen(*)
            real(kind=8) :: bmodal(neq,*)
            integer :: info
            logical :: lflu
            character(len=8) :: monmot
            integer :: ier
          end subroutine mdchoc
        end interface
