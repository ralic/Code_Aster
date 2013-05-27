        interface
          subroutine nmbarc(ndim,imate,crit,sat,biot,tm,tp,deps,sbism,&
     &vim,option,sbisp,vip,dsidep,p1,p2,dp1,dp2,dsidp1,sipm,sipp,retcom)
            integer :: ndim
            integer :: imate
            real(kind=8) :: crit(*)
            real(kind=8) :: sat
            real(kind=8) :: biot
            real(kind=8) :: tm
            real(kind=8) :: tp
            real(kind=8) :: deps(6)
            real(kind=8) :: sbism(6)
            real(kind=8) :: vim(5)
            character(len=16) :: option
            real(kind=8) :: sbisp(6)
            real(kind=8) :: vip(5)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: p1
            real(kind=8) :: p2
            real(kind=8) :: dp1
            real(kind=8) :: dp2
            real(kind=8) :: dsidp1(6)
            real(kind=8) :: sipm
            real(kind=8) :: sipp
            integer :: retcom
          end subroutine nmbarc
        end interface
