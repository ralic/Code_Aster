        interface
          subroutine fetrin(nbsd,nbi,vdo,vd1,matas,vsdf,vddl,colaux,&
     &chsecm,sdfeti,vlagi,option,chsol,testco,lrigid,dimgi,irr,nomggt,&
     &ipiv,nomgi,lstogi,infofe,irex,iprj,ifm,ifiv,nbproc,rang,k24irr)
            integer :: nbi
            integer :: nbsd
            real(kind=8) :: vdo(nbi)
            real(kind=8) :: vd1(nbi)
            character(len=19) :: matas
            integer :: vsdf(nbsd)
            integer :: vddl(nbsd)
            character(len=24) :: colaux
            character(len=19) :: chsecm
            character(len=19) :: sdfeti
            real(kind=8) :: vlagi(nbi)
            integer :: option
            character(len=19) :: chsol
            real(kind=8) :: testco
            logical :: lrigid
            integer :: dimgi
            integer :: irr
            character(len=24) :: nomggt
            integer :: ipiv
            character(len=24) :: nomgi
            logical :: lstogi
            character(len=24) :: infofe
            integer :: irex
            integer :: iprj
            integer :: ifm
            integer :: ifiv
            integer :: nbproc
            integer :: rang
            character(len=24) :: k24irr
          end subroutine fetrin
        end interface
