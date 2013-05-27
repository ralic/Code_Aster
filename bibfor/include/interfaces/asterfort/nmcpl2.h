        interface
          subroutine nmcpl2(compor,typmod,option,optio2,cp,nvv,crit,&
     &deps,dsidep,ndim,sigp,vip,iret)
            character(len=16) :: compor(*)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            character(len=16) :: optio2
            integer :: cp
            integer :: nvv
            real(kind=8) :: crit(*)
            real(kind=8) :: deps(*)
            real(kind=8) :: dsidep(6,*)
            integer :: ndim
            real(kind=8) :: sigp(*)
            real(kind=8) :: vip(*)
            integer :: iret
          end subroutine nmcpl2
        end interface
