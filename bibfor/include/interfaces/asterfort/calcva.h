        interface
          subroutine calcva(kpi,yachai,yamec,yate,yap1,yap2,defgem,&
     &defgep,addeme,addep1,addep2,addete,ndim,t0,p10,p20,depsv,epsv,deps&
     &,t,p1,p2,grat,grap1,grap2,dp1,dp2,dt,retcom)
            integer :: ndim
            integer :: kpi
            logical :: yachai
            integer :: yamec
            integer :: yate
            integer :: yap1
            integer :: yap2
            real(kind=8) :: defgem(*)
            real(kind=8) :: defgep(*)
            integer :: addeme
            integer :: addep1
            integer :: addep2
            integer :: addete
            real(kind=8) :: t0
            real(kind=8) :: p10
            real(kind=8) :: p20
            real(kind=8) :: depsv
            real(kind=8) :: epsv
            real(kind=8) :: deps(6)
            real(kind=8) :: t
            real(kind=8) :: p1
            real(kind=8) :: p2
            real(kind=8) :: grat(ndim)
            real(kind=8) :: grap1(ndim)
            real(kind=8) :: grap2(ndim)
            real(kind=8) :: dp1
            real(kind=8) :: dp2
            real(kind=8) :: dt
            integer :: retcom
          end subroutine calcva
        end interface
