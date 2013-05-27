        interface
          subroutine fetinl(nbi,vlagi,matas,chsecm,lrigid,dimgi,nbsd,&
     &vsdf,vddl,nomggt,ipiv,nomgi,lstogi,infofe,irex,ifm,sdfeti,nbproc,&
     &rang,k24lai,itps)
            integer :: nbsd
            integer :: nbi
            real(kind=8) :: vlagi(nbi)
            character(len=19) :: matas
            character(len=19) :: chsecm
            logical :: lrigid
            integer :: dimgi
            integer :: vsdf(nbsd)
            integer :: vddl(nbsd)
            character(len=24) :: nomggt
            integer :: ipiv
            character(len=24) :: nomgi
            logical :: lstogi
            character(len=24) :: infofe
            integer :: irex
            integer :: ifm
            character(len=19) :: sdfeti
            integer :: nbproc
            integer :: rang
            character(len=24) :: k24lai
            integer :: itps
          end subroutine fetinl
        end interface
