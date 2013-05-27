        interface
          subroutine fettsd(infofe,nbi,nbsd,vddl,sdfeti,colaux,irex,&
     &nbi2,ifeti,ifm,lpara,itps,nivmpi,rang,chsol,option,ltest)
            integer :: nbsd
            character(len=24) :: infofe
            integer :: nbi
            integer :: vddl(nbsd)
            character(len=19) :: sdfeti
            character(len=24) :: colaux
            integer :: irex
            integer :: nbi2
            integer :: ifeti
            integer :: ifm
            logical :: lpara
            integer :: itps
            integer :: nivmpi
            integer :: rang
            character(len=19) :: chsol
            integer :: option
            logical :: ltest
          end subroutine fettsd
        end interface
