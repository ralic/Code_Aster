        interface
          subroutine nmgvno(fami,ndim,nno1,nno2,npg,iw,vff1,vff2,&
     &idfde1,idfde2,geom,typmod,option,mat,compor,lgpg,crit,instam,&
     &instap,ddlm,ddld,angmas,sigm,vim,sigp,vip,matr,vect,codret)
            integer :: lgpg
            integer :: npg
            integer :: nno2
            integer :: nno1
            integer :: ndim
            character(*) :: fami
            integer :: iw
            real(kind=8) :: vff1(nno1,npg)
            real(kind=8) :: vff2(nno2,npg)
            integer :: idfde1
            integer :: idfde2
            real(kind=8) :: geom(ndim,nno1)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            integer :: mat
            character(len=16) :: compor(*)
            real(kind=8) :: crit(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: ddlm(*)
            real(kind=8) :: ddld(*)
            real(kind=8) :: angmas(3)
            real(kind=8) :: sigm(2*ndim+1,npg)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: sigp(2*ndim+1,npg)
            real(kind=8) :: vip(lgpg,npg)
            real(kind=8) :: matr(*)
            real(kind=8) :: vect(*)
            integer :: codret
          end subroutine nmgvno
        end interface
