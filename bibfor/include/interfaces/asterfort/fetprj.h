        interface
          subroutine fetprj(nbi,vi,vo,nomggt,lrigid,dimgi,option,&
     &sdfeti,ipiv,nbsd,vsdf,vddl,matas,nomgi,lstogi,infofe,irex,iprj,&
     &nbproc,rang,k24irg)
            integer :: nbsd
            integer :: nbi
            real(kind=8) :: vi(nbi)
            real(kind=8) :: vo(nbi)
            character(len=24) :: nomggt
            logical :: lrigid
            integer :: dimgi
            integer :: option
            character(len=19) :: sdfeti
            integer :: ipiv
            integer :: vsdf(nbsd)
            integer :: vddl(nbsd)
            character(len=19) :: matas
            character(len=24) :: nomgi
            logical :: lstogi
            character(len=24) :: infofe
            integer :: irex
            integer :: iprj
            integer :: nbproc
            integer :: rang
            character(len=24) :: k24irg
          end subroutine fetprj
        end interface
