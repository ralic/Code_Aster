        interface
          subroutine fetarp(infofe,ifm,niter,nbi,nbreor,lrigid,dimgi,&
     &sdfeti,ipiv,nomggt,nbsd,ifetf,ifeth,nomgi,lstogi,irex,iprj,ir2,&
     &ifiv,matas,nbproc,rang)
            character(len=24) :: infofe
            integer :: ifm
            integer :: niter
            integer :: nbi
            integer :: nbreor
            logical :: lrigid
            integer :: dimgi
            character(len=19) :: sdfeti
            integer :: ipiv
            character(len=24) :: nomggt
            integer :: nbsd
            integer :: ifetf
            integer :: ifeth
            character(len=24) :: nomgi
            logical :: lstogi
            integer :: irex
            integer :: iprj
            integer :: ir2
            integer :: ifiv
            character(len=19) :: matas
            integer :: nbproc
            integer :: rang
          end subroutine fetarp
        end interface
