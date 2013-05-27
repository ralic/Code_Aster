        interface
          subroutine dpvplc(typmod,option,imate,crit,instam,instap,td,&
     &tf,tr,depsm,sigm,vim,sig,vip,dsidep,iret)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            integer :: imate
            real(kind=8) :: crit(3)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: td
            real(kind=8) :: tf
            real(kind=8) :: tr
            real(kind=8) :: depsm(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(*)
            real(kind=8) :: sig(6)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep(6,6)
            integer :: iret
          end subroutine dpvplc
        end interface
