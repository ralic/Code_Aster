        interface
          subroutine mdrecf(nexci,nexcir,idescf,nomfon,coefm,iadvec,&
     &inumor,fondep,fonvit,fonacc,neq,typbas,basemo,nbmode,riggen,nommot&
     &,nomres)
            integer :: nbmode
            integer :: nexci
            integer :: nexcir
            integer :: idescf(*)
            character(len=8) :: nomfon(2*nexci)
            real(kind=8) :: coefm(*)
            integer :: iadvec(*)
            integer :: inumor(*)
            character(len=8) :: fondep(2*nexci)
            character(len=8) :: fonvit(2*nexci)
            character(len=8) :: fonacc(2*nexci)
            integer :: neq
            character(len=16) :: typbas
            character(len=8) :: basemo
            real(kind=8) :: riggen(nbmode)
            character(len=8) :: nommot
            character(len=8) :: nomres
          end subroutine mdrecf
        end interface
