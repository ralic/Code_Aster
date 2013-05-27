        interface
          subroutine cricho(nbmode,riggen,nbchoc,parcho,noecho,info,&
     &fimpo,rfimpo,trloc,soupl,indic,neq,bmodal,seuil,marig,nbnli)
            integer :: nbnli
            integer :: neq
            integer :: nbmode
            real(kind=8) :: riggen(*)
            integer :: nbchoc
            real(kind=8) :: parcho(nbnli,*)
            character(len=8) :: noecho(nbnli,*)
            integer :: info
            real(kind=8) :: fimpo(neq)
            real(kind=8) :: rfimpo(neq)
            real(kind=8) :: trloc(nbmode)
            real(kind=8) :: soupl(nbmode)
            integer :: indic(nbmode)
            real(kind=8) :: bmodal(neq,nbmode)
            real(kind=8) :: seuil
            character(len=19) :: marig
          end subroutine cricho
        end interface
