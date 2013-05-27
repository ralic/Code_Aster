        interface
          subroutine ngfint(option,typmod,ndim,nddl,neps,npg,w,b,&
     &compor,fami,mat,angmas,lgpg,crit,instam,instap,ddlm,ddld,ni2ldc,&
     &sigmam,vim,sigmap,vip,fint,matr,codret)
            integer :: lgpg
            integer :: npg
            integer :: neps
            integer :: nddl
            character(len=16) :: option
            character(len=8) :: typmod(*)
            integer :: ndim
            real(kind=8) :: w(0:npg-1)
            real(kind=8) :: b(neps,npg,nddl)
            character(len=16) :: compor(*)
            character(*) :: fami
            integer :: mat
            real(kind=8) :: angmas(3)
            real(kind=8) :: crit(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: ddlm(nddl)
            real(kind=8) :: ddld(nddl)
            real(kind=8) :: ni2ldc(0:neps-1)
            real(kind=8) :: sigmam(0:neps*npg-1)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: sigmap(0:neps*npg-1)
            real(kind=8) :: vip(lgpg,npg)
            real(kind=8) :: fint(nddl)
            real(kind=8) :: matr(nddl,nddl)
            integer :: codret
          end subroutine ngfint
        end interface
