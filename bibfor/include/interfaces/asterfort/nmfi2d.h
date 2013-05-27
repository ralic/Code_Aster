        interface
          subroutine nmfi2d(npg,lgpg,mate,option,geom,deplm,ddepl,&
     &sigmo,sigma,fint,ktan,vim,vip,tm,tp,crit,compor,typmod,codret)
            integer :: lgpg
            integer :: npg
            integer :: mate
            character(len=16) :: option
            real(kind=8) :: geom(2,4)
            real(kind=8) :: deplm(8)
            real(kind=8) :: ddepl(8)
            real(kind=8) :: sigmo(6,npg)
            real(kind=8) :: sigma(6,npg)
            real(kind=8) :: fint(8)
            real(kind=8) :: ktan(8,8)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: vip(lgpg,npg)
            real(kind=8) :: tm
            real(kind=8) :: tp
            real(kind=8) :: crit(*)
            character(len=16) :: compor(*)
            character(len=8) :: typmod(*)
            integer :: codret
          end subroutine nmfi2d
        end interface
