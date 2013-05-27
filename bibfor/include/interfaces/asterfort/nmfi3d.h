        interface
          subroutine nmfi3d(nno,nddl,npg,lgpg,wref,vff,dfde,mate,&
     &option,geom,deplm,ddepl,sigm,sigp,fint,ktan,vim,vip,crit,compor,&
     &matsym,coopg,tm,tp,codret)
            integer :: lgpg
            integer :: npg
            integer :: nddl
            integer :: nno
            real(kind=8) :: wref(npg)
            real(kind=8) :: vff(nno,npg)
            real(kind=8) :: dfde(2,nno,npg)
            integer :: mate
            character(len=16) :: option
            real(kind=8) :: geom(nddl)
            real(kind=8) :: deplm(nddl)
            real(kind=8) :: ddepl(nddl)
            real(kind=8) :: sigm(3,npg)
            real(kind=8) :: sigp(3,npg)
            real(kind=8) :: fint(nddl)
            real(kind=8) :: ktan(*)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: vip(lgpg,npg)
            real(kind=8) :: crit(*)
            character(len=16) :: compor(*)
            logical :: matsym
            real(kind=8) :: coopg(4,npg)
            real(kind=8) :: tm
            real(kind=8) :: tp
            integer :: codret
          end subroutine nmfi3d
        end interface
