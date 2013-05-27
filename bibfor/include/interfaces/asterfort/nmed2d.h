        interface
          subroutine nmed2d(nno,npg,ipoids,ivf,idfde,geom,typmod,&
     &option,imate,compor,lgpg,crit,ideplm,iddepl,sigm,vim,dfdi,def,sigp&
     &,vip,matuu,ivectu,codret)
            integer :: lgpg
            integer :: npg
            integer :: nno
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: geom(2,nno)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            integer :: imate
            character(len=16) :: compor(4)
            real(kind=8) :: crit(3)
            integer :: ideplm
            integer :: iddepl
            real(kind=8) :: sigm(4,npg)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: dfdi(nno,2)
            real(kind=8) :: def(4,nno,2)
            real(kind=8) :: sigp(4,npg)
            real(kind=8) :: vip(lgpg,npg)
            real(kind=8) :: matuu(*)
            integer :: ivectu
            integer :: codret
          end subroutine nmed2d
        end interface
