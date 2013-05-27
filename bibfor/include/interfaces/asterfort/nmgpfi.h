        interface
          subroutine nmgpfi(fami,option,typmod,ndim,nno,npg,iw,vff,&
     &idff,geomi,dff,compor,mate,lgpg,crit,angmas,instm,instp,deplm,&
     &depld,sigm,vim,sigp,vip,fint,matr,codret)
            integer :: lgpg
            integer :: npg
            integer :: nno
            integer :: ndim
            character(*) :: fami
            character(len=16) :: option
            character(len=8) :: typmod(*)
            integer :: iw
            real(kind=8) :: vff(nno,npg)
            integer :: idff
            real(kind=8) :: geomi(*)
            real(kind=8) :: dff(nno,*)
            character(len=16) :: compor(*)
            integer :: mate
            real(kind=8) :: crit(*)
            real(kind=8) :: angmas(3)
            real(kind=8) :: instm
            real(kind=8) :: instp
            real(kind=8) :: deplm(*)
            real(kind=8) :: depld(*)
            real(kind=8) :: sigm(2*ndim,npg)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: sigp(2*ndim,npg)
            real(kind=8) :: vip(lgpg,npg)
            real(kind=8) :: fint(*)
            real(kind=8) :: matr(*)
            integer :: codret
          end subroutine nmgpfi
        end interface
