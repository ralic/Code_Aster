        interface
          subroutine lgdmvm(imate,compor,epsm,deps,vim,option,sigm,sig&
     &,vip,dsidep,crit,iret)
            integer :: imate
            character(len=16) :: compor
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vim(*)
            character(len=16) :: option
            real(kind=8) :: sigm(*)
            real(kind=8) :: sig(*)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidep(6,*)
            real(kind=8) :: crit(*)
            integer :: iret
          end subroutine lgdmvm
        end interface
