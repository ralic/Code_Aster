        interface
          subroutine fetacc(option,rang,dimtet,imsmi,imsmk,nbreoa,itps&
     &,irg,irr,ivlagi,nbi,ir1,ir2,ir3,nomggt,lrigid,dimgi,sdfeti,ipiv,&
     &nbsd,vsdf,vddl,matas,nomgi,lstogi,infofe,irex,iprj,nbproc)
            integer :: nbsd
            integer :: option
            integer :: rang
            integer :: dimtet
            integer :: imsmi
            integer :: imsmk
            integer :: nbreoa
            integer :: itps
            integer :: irg
            integer :: irr
            integer :: ivlagi
            integer :: nbi
            integer :: ir1
            integer :: ir2
            integer :: ir3
            character(len=24) :: nomggt
            logical :: lrigid
            integer :: dimgi
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
          end subroutine fetacc
        end interface
