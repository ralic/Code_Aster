        interface
          subroutine umatwp(nomlib,nomsub,stress,statev,ddsdde,sse,spd&
     &,scd,rpl,ddsddt,drplde,drpldt,stran,dstran,time,dtime,temp,dtemp,&
     &predef,dpred,cmname,ndi,nshr,ntens,nstatv,props,nprops,coords,drot&
     &,pnewdt,celent,dfgrd0,dfgrd1,noel,npt,layer,kspt,kstep,kinc)
            character(*) :: nomlib
            character(*) :: nomsub
            real(kind=8) :: stress(*)
            real(kind=8) :: statev(*)
            real(kind=8) :: ddsdde(*)
            real(kind=8) :: sse(*)
            real(kind=8) :: spd(*)
            real(kind=8) :: scd(*)
            real(kind=8) :: rpl(*)
            real(kind=8) :: ddsddt(*)
            real(kind=8) :: drplde(*)
            real(kind=8) :: drpldt(*)
            real(kind=8) :: stran(*)
            real(kind=8) :: dstran(*)
            real(kind=8) :: time(*)
            real(kind=8) :: dtime(*)
            real(kind=8) :: temp(*)
            real(kind=8) :: dtemp(*)
            real(kind=8) :: predef(*)
            real(kind=8) :: dpred(*)
            character(*) :: cmname
            integer :: ndi
            integer :: nshr
            integer :: ntens
            integer :: nstatv
            real(kind=8) :: props
            integer :: nprops
            real(kind=8) :: coords(*)
            real(kind=8) :: drot(*)
            real(kind=8) :: pnewdt(*)
            real(kind=8) :: celent(*)
            real(kind=8) :: dfgrd0(*)
            real(kind=8) :: dfgrd1(*)
            integer :: noel
            integer :: npt
            integer :: layer
            integer :: kspt
            integer :: kstep
            integer :: kinc
          end subroutine umatwp
        end interface
