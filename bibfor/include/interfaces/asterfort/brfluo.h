        interface
          subroutine brfluo(sut,sut6,xmt,sige6,eps0,tau0,dt,evp06,&
     &evp16,devpt6,evpmax,bgpgmx,eragmx)
            real(kind=8) :: sut
            real(kind=8) :: sut6(6)
            real(kind=8) :: xmt
            real(kind=8) :: sige6(6)
            real(kind=8) :: eps0
            real(kind=8) :: tau0
            real(kind=8) :: dt
            real(kind=8) :: evp06(6)
            real(kind=8) :: evp16(6)
            real(kind=8) :: devpt6(6)
            real(kind=8) :: evpmax
            real(kind=8) :: bgpgmx
            real(kind=8) :: eragmx
          end subroutine brfluo
        end interface
