        interface
          subroutine lchbr2(typmod,option,imate,crit,sigm,epsm,td,tf,&
     &tr,depsm,vim,vip,dspdp1,dspdp2,sipp,sigp,dsidep,dsidp1,dsidp2,iret&
     &)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            integer :: imate
            real(kind=8) :: crit(*)
            real(kind=8) :: sigm(6)
            real(kind=8) :: epsm(6)
            real(kind=8) :: td
            real(kind=8) :: tf
            real(kind=8) :: tr
            real(kind=8) :: depsm(6)
            real(kind=8) :: vim(*)
            real(kind=8) :: vip(*)
            real(kind=8) :: dspdp1
            real(kind=8) :: dspdp2
            real(kind=8) :: sipp
            real(kind=8) :: sigp(6)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: dsidp1(6)
            real(kind=8) :: dsidp2(6)
            integer :: iret
          end subroutine lchbr2
        end interface
