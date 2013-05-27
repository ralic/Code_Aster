        interface
          subroutine fetfiv(nbsd,nbi,vd1,vd2,vdo,matas,vsdf,vddl,&
     &infofe,irex,ifiv,nbproc,rang,k24irz,sdfeti)
            integer :: nbi
            integer :: nbsd
            real(kind=8) :: vd1(nbi)
            real(kind=8) :: vd2(nbi)
            real(kind=8) :: vdo(nbi)
            character(len=19) :: matas
            integer :: vsdf(nbsd)
            integer :: vddl(nbsd)
            character(len=24) :: infofe
            integer :: irex
            integer :: ifiv
            integer :: nbproc
            integer :: rang
            character(len=24) :: k24irz
            character(len=19) :: sdfeti
          end subroutine fetfiv
        end interface
