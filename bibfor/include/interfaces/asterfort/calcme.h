        interface
          subroutine calcme(option,compor,thmc,meca,imate,typmod,crit,&
     &instam,instap,tref,ndim,dimdef,dimcon,nvimec,yate,addeme,adcome,&
     &addete,defgem,congem,congep,vintm,vintp,addep1,addep2,dsde,deps,&
     &depsv,p1,p2,t,dt,retcom,dp1,dp2,sat,biot)
            integer :: nvimec
            integer :: dimcon
            integer :: dimdef
            character(len=16) :: option
            character(len=16) :: compor(*)
            character(len=16) :: thmc
            character(len=16) :: meca
            integer :: imate
            character(len=8) :: typmod(2)
            real(kind=8) :: crit(*)
            real(kind=8) :: instam
            real(kind=8) :: instap
            real(kind=8) :: tref
            integer :: ndim
            integer :: yate
            integer :: addeme
            integer :: adcome
            integer :: addete
            real(kind=8) :: defgem(dimdef)
            real(kind=8) :: congem(dimcon)
            real(kind=8) :: congep(dimcon)
            real(kind=8) :: vintm(nvimec)
            real(kind=8) :: vintp(nvimec)
            integer :: addep1
            integer :: addep2
            real(kind=8) :: dsde(dimcon,dimdef)
            real(kind=8) :: deps(6)
            real(kind=8) :: depsv
            real(kind=8) :: p1
            real(kind=8) :: p2
            real(kind=8) :: t
            real(kind=8) :: dt
            integer :: retcom
            real(kind=8) :: dp1
            real(kind=8) :: dp2
            real(kind=8) :: sat
            real(kind=8) :: biot
          end subroutine calcme
        end interface
