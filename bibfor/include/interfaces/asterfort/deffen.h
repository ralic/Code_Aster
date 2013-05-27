        interface
          subroutine deffen(base,nuor,imodi,nbmr,nbm,iaxe,long,nbnfen,&
     &nofe,discfe,nbp1,nbp2,discff,defm)
            integer :: nbp2
            integer :: nbp1
            integer :: nbnfen
            integer :: nbm
            integer :: nbmr
            character(len=19) :: base
            integer :: nuor(nbm)
            integer :: imodi
            integer :: iaxe
            real(kind=8) :: long
            integer :: nofe(nbnfen)
            real(kind=8) :: discfe(nbnfen)
            real(kind=8) :: discff(nbp1+nbp2)
            real(kind=8) :: defm(nbp1+nbp2,nbmr)
          end subroutine deffen
        end interface
