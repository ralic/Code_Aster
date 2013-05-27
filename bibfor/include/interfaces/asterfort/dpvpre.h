        interface
          subroutine dpvpre(mod,nvi,option,crit,instam,instap,nbmat,&
     &materf,sigm,deps,vim,vip,sig,nbre,dsidep,iret)
            integer :: nbmat
            integer :: nvi
            character(len=8) :: mod
            character(len=16) :: option
            real(kind=8) :: crit(3)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: materf(nbmat,2)
            real(kind=8) :: sigm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vim(nvi)
            real(kind=8) :: vip(nvi)
            real(kind=8) :: sig(6)
            integer :: nbre
            real(kind=8) :: dsidep(6,6)
            integer :: iret
          end subroutine dpvpre
        end interface
