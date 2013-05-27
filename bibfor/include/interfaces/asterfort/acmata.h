        interface
          subroutine acmata(jvectn,jvectu,jvectv,nbordr,kwork,sompgw,&
     &jrwork,tspaq,ipg,jvecpg,jdtaum,jresun,nommet,vrespc)
            integer :: jvectn
            integer :: jvectu
            integer :: jvectv
            integer :: nbordr
            integer :: kwork
            integer :: sompgw
            integer :: jrwork
            integer :: tspaq
            integer :: ipg
            integer :: jvecpg
            integer :: jdtaum
            integer :: jresun
            character(len=16) :: nommet
            real(kind=8) :: vrespc(24)
          end subroutine acmata
        end interface
