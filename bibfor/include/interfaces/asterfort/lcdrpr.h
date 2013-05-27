        interface
          subroutine lcdrpr(typmod,option,imate,compor,sigm,td,tf,tr,&
     &depsm,vim,vip,sig,dsidep,iret)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            integer :: imate
            character(len=16) :: compor(*)
            real(kind=8) :: sigm(6)
            real(kind=8) :: td
            real(kind=8) :: tf
            real(kind=8) :: tr
            real(kind=8) :: depsm(6)
            real(kind=8) :: vim(*)
            real(kind=8) :: vip(*)
            real(kind=8) :: sig(6)
            real(kind=8) :: dsidep(6,6)
            integer :: iret
          end subroutine lcdrpr
        end interface
