        interface
          subroutine fetggt(nbsd,matas,vsdf,vddl,lrigid,nbi,nomggt,&
     &dimgi,nomgi,stogi,lstogi,mamoy,infofe,irex,ifm,sdfeti,nbproc,rang,&
     &itps)
            integer :: nbsd
            character(len=19) :: matas
            integer :: vsdf(nbsd)
            integer :: vddl(nbsd)
            logical :: lrigid
            integer :: nbi
            character(len=24) :: nomggt
            integer :: dimgi
            character(len=24) :: nomgi
            character(len=24) :: stogi
            logical :: lstogi
            integer :: mamoy
            character(len=24) :: infofe
            integer :: irex
            integer :: ifm
            character(len=19) :: sdfeti
            integer :: nbproc
            integer :: rang
            integer :: itps
          end subroutine fetggt
        end interface
