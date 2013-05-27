        interface
          subroutine fetsca(nbi,vi,vo,scalin,infofe,nbi2,ifeti,ifm)
            integer :: nbi
            real(kind=8) :: vi(nbi)
            real(kind=8) :: vo(nbi)
            character(len=24) :: scalin
            character(len=24) :: infofe
            integer :: nbi2
            integer :: ifeti
            integer :: ifm
          end subroutine fetsca
        end interface
