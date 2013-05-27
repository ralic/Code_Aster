        interface
          subroutine lcjoba(ndim,typmod,imate,crit,sum,dsu,vim,option,&
     &sig,vip,dsidep,iret)
            integer :: ndim
            character(len=8) :: typmod(2)
            integer :: imate
            real(kind=8) :: crit(3)
            real(kind=8) :: sum(2)
            real(kind=8) :: dsu(2)
            real(kind=8) :: vim(6)
            character(len=16) :: option
            real(kind=8) :: sig(6)
            real(kind=8) :: vip(6)
            real(kind=8) :: dsidep(6,6)
            integer :: iret
          end subroutine lcjoba
        end interface
