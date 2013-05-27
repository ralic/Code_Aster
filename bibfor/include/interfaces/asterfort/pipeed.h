        interface
          subroutine pipeed(nno,npg,ipoids,ivf,idfde,geom,typmod,mate,&
     &lgpg,deplm,vim,ddepl,ddepl0,ddepl1,dfdi,dtau,copilo)
            integer :: lgpg
            integer :: npg
            integer :: nno
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: geom(2,4)
            character(len=8) :: typmod(*)
            integer :: mate
            real(kind=8) :: deplm(2,4)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: ddepl(2,4)
            real(kind=8) :: ddepl0(2,4)
            real(kind=8) :: ddepl1(2,4)
            real(kind=8) :: dfdi(nno,2)
            real(kind=8) :: dtau
            real(kind=8) :: copilo(5,npg)
          end subroutine pipeed
        end interface
