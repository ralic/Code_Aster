        interface
          subroutine nmcpl3(compor,option,crit,deps,dsidep,ndim,sigp,&
     &vip,cpl,icp,conv)
            character(len=16) :: compor(*)
            character(len=16) :: option
            real(kind=8) :: crit(*)
            real(kind=8) :: deps(*)
            real(kind=8) :: dsidep(6,6)
            integer :: ndim
            real(kind=8) :: sigp(4)
            real(kind=8) :: vip(*)
            integer :: cpl
            integer :: icp
            logical :: conv
          end subroutine nmcpl3
        end interface
