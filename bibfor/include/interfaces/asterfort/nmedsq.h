        interface
          subroutine nmedsq(sg,qg,dsdug,d,npg,typmod,imate,bum,bdu,&
     &sign,vim,option,geom,nno,lgpg,kpg,def)
            integer :: lgpg
            integer :: nno
            integer :: npg
            real(kind=8) :: sg(2)
            real(kind=8) :: qg(2,*)
            real(kind=8) :: dsdug(2,8)
            real(kind=8) :: d(4,*)
            character(len=8) :: typmod(*)
            integer :: imate
            real(kind=8) :: bum(6)
            real(kind=8) :: bdu(6)
            real(kind=8) :: sign(*)
            real(kind=8) :: vim(lgpg,npg)
            character(len=16) :: option
            real(kind=8) :: geom(2,nno)
            integer :: kpg
            real(kind=8) :: def(4,nno,2)
          end subroutine nmedsq
        end interface
