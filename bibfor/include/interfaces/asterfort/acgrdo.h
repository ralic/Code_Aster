        interface
          subroutine acgrdo(jvectn,jvectu,jvectv,nbordr,ordini,kwork,&
     &sompgw,jrwork,tspaq,ipg,jvecpg,jdtaum,jresun,nommet,nommat,nomcri,&
     &vala,coefpa,nomfor,grdvie,forvie,valpar,vresu)
            integer :: nbordr
            integer :: jvectn
            integer :: jvectu
            integer :: jvectv
            integer :: ordini
            integer :: kwork
            integer :: sompgw
            integer :: jrwork
            integer :: tspaq
            integer :: ipg
            integer :: jvecpg
            integer :: jdtaum
            integer :: jresun
            character(len=16) :: nommet
            character(len=8) :: nommat
            character(len=16) :: nomcri
            real(kind=8) :: vala
            real(kind=8) :: coefpa
            character(len=16) :: nomfor
            character(len=8) :: grdvie
            character(len=16) :: forvie
            real(kind=8) :: valpar(22)
            real(kind=8) :: vresu(24)
          end subroutine acgrdo
        end interface
