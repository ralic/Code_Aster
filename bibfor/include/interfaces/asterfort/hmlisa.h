        interface
          subroutine hmlisa(perman,yachai,option,meca,thmc,ther,hydr,&
     &imate,ndim,dimdef,dimcon,nbvari,yamec,yate,addeme,adcome,advihy,&
     &advico,vihrho,vicphi,addep1,adcp11,addete,adcote,congem,congep,&
     &vintm,vintp,dsde,epsv,depsv,p1,dp1,t,dt,phi,rho11,phi0,sat,retcom,&
     &biot,rinstp)
            integer :: nbvari
            integer :: dimcon
            integer :: dimdef
            logical :: perman
            logical :: yachai
            character(len=16) :: option
            character(len=16) :: meca
            character(len=16) :: thmc
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
            integer :: addep1
            integer :: adcp11
            integer :: addete
            integer :: adcote
            real(kind=8) :: congem(dimcon)
            real(kind=8) :: congep(dimcon)
            real(kind=8) :: vintm(nbvari)
            real(kind=8) :: vintp(nbvari)
            real(kind=8) :: dsde(dimcon,dimdef)
            real(kind=8) :: epsv
            real(kind=8) :: depsv
            real(kind=8) :: p1
            real(kind=8) :: dp1
            real(kind=8) :: t
            real(kind=8) :: dt
            real(kind=8) :: phi
            real(kind=8) :: rho11
            real(kind=8) :: phi0
            real(kind=8) :: sat
            integer :: retcom
            real(kind=8) :: biot
            real(kind=8) :: rinstp
          end subroutine hmlisa
        end interface
