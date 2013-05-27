        interface
          subroutine lcgdpm(fami,kpg,ksp,ndim,imat,compor,crit,instam,&
     &instap,fm,df,sigm,vim,option,sigp,vip,dsigdf,iret)
            character(*) :: fami
            integer :: kpg
            integer :: ksp
            integer :: ndim
            integer :: imat
            character(len=16) :: compor(3)
            real(kind=8) :: crit(3)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: fm(3,3)
            real(kind=8) :: df(3,3)
            real(kind=8) :: sigm(*)
            real(kind=8) :: vim(8)
            character(len=16) :: option
            real(kind=8) :: sigp(*)
            real(kind=8) :: vip(8)
            real(kind=8) :: dsigdf(6,3,3)
            integer :: iret
          end subroutine lcgdpm
        end interface
