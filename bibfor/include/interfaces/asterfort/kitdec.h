        interface
          subroutine kitdec(kpi,yachai,yamec,yate,yap1,yap2,meca,thmc,&
     &ther,hydr,imate,defgem,defgep,addeme,addep1,addep2,addete,ndim,t0,&
     &p10,p20,phi0,pvp0,depsv,epsv,deps,t,p1,p2,dt,dp1,dp2,grat,grap1,&
     &grap2,retcom,rinstp)
            integer :: kpi
            logical :: yachai
            integer :: yamec
            integer :: yate
            integer :: yap1
            integer :: yap2
            character(len=16) :: meca
            character(len=16) :: thmc
            character(len=16) :: ther
            character(len=16) :: hydr
            integer :: imate
            real(kind=8) :: defgem(*)
            real(kind=8) :: defgep(*)
            integer :: addeme
            integer :: addep1
            integer :: addep2
            integer :: addete
            integer :: ndim
            real(kind=8) :: t0
            real(kind=8) :: p10
            real(kind=8) :: p20
            real(kind=8) :: phi0
            real(kind=8) :: pvp0
            real(kind=8) :: depsv
            real(kind=8) :: epsv
            real(kind=8) :: deps(6)
            real(kind=8) :: t
            real(kind=8) :: p1
            real(kind=8) :: p2
            real(kind=8) :: dt
            real(kind=8) :: dp1
            real(kind=8) :: dp2
            real(kind=8) :: grat(3)
            real(kind=8) :: grap1(3)
            real(kind=8) :: grap2(3)
            integer :: retcom
            real(kind=8) :: rinstp
          end subroutine kitdec
        end interface
