        interface
          subroutine hmlvag(yachai,option,meca,ther,hydr,imate,ndim,&
     &dimdef,dimcon,nbvari,yamec,yate,addeme,adcome,advihy,advico,vihrho&
     &,vicphi,vicpvp,vicsat,addep1,adcp11,adcp12,addep2,adcp21,addete,&
     &adcote,congem,congep,vintm,vintp,dsde,deps,epsv,depsv,p1,p2,dp1,&
     &dp2,t,dt,phi,pvp,h11,h12,rho11,phi0,pvp0,sat,retcom,thmc,crit,biot&
     &,rinstp)
            integer :: nbvari
            integer :: dimcon
            integer :: dimdef
            logical :: yachai
            character(len=16) :: option
            character(len=16) :: meca
            character(len=16) :: ther
            character(len=16) :: hydr
            integer :: imate
            integer :: ndim
            integer :: yamec
            integer :: yate
            integer :: addeme
            integer :: adcome
            integer :: advihy
            integer :: advico
            integer :: vihrho
            integer :: vicphi
            integer :: vicpvp
            integer :: vicsat
            integer :: addep1
            integer :: adcp11
            integer :: adcp12
            integer :: addep2
            integer :: adcp21
            integer :: addete
            integer :: adcote
            real(kind=8) :: congem(dimcon)
            real(kind=8) :: congep(dimcon)
            real(kind=8) :: vintm(nbvari)
            real(kind=8) :: vintp(nbvari)
            real(kind=8) :: dsde(dimcon,dimdef)
            real(kind=8) :: deps(6)
            real(kind=8) :: epsv
            real(kind=8) :: depsv
            real(kind=8) :: p1
            real(kind=8) :: p2
            real(kind=8) :: dp1
            real(kind=8) :: dp2
            real(kind=8) :: t
            real(kind=8) :: dt
            real(kind=8) :: phi
            real(kind=8) :: pvp
            real(kind=8) :: h11
            real(kind=8) :: h12
            real(kind=8) :: rho11
            real(kind=8) :: phi0
            real(kind=8) :: pvp0
            real(kind=8) :: sat
            integer :: retcom
            character(len=16) :: thmc
            real(kind=8) :: crit(*)
            real(kind=8) :: biot
            real(kind=8) :: rinstp
          end subroutine hmlvag
        end interface
