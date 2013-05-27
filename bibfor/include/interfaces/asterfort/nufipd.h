        interface
          subroutine nufipd(ndim,nno1,nno2,npg,iw,vff1,vff2,idff1,vu,&
     &vp,geomi,typmod,option,mate,compor,lgpg,crit,instm,instp,ddlm,ddld&
     &,angmas,sigm,vim,sigp,vip,resi,rigi,mini,vect,matr,codret)
            integer :: lgpg
            integer :: npg
            integer :: nno2
            integer :: nno1
            integer :: ndim
            integer :: iw
            real(kind=8) :: vff1(nno1,npg)
            real(kind=8) :: vff2(nno2,npg)
            integer :: idff1
            integer :: vu(3,27)
            integer :: vp(27)
            real(kind=8) :: geomi(ndim,nno1)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            integer :: mate
            character(len=16) :: compor(*)
            real(kind=8) :: crit(*)
            real(kind=8) :: instm
            real(kind=8) :: instp
            real(kind=8) :: ddlm(*)
            real(kind=8) :: ddld(*)
            real(kind=8) :: angmas(*)
            real(kind=8) :: sigm(2*ndim+1,npg)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: sigp(2*ndim+1,npg)
            real(kind=8) :: vip(lgpg,npg)
            logical :: resi
            logical :: rigi
            logical :: mini
            real(kind=8) :: vect(*)
            real(kind=8) :: matr(*)
            integer :: codret
          end subroutine nufipd
        end interface
