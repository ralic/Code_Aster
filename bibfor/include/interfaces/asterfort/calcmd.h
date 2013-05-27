        interface
          subroutine calcmd(np1,a,a0,n,m,typj,vg,vgt,vgt0,vd,vd0,rr,&
     &rr0,ri,n2,ier,ichoc,premac,prerel,mtmp1,mtmp2,ttr,u,w,d,intge1,&
     &intge2,indx,indxf,loc)
            integer :: n2
            integer :: np1
            real(kind=8) :: a(np1,*)
            real(kind=8) :: a0(np1,*)
            integer :: n
            integer :: m
            integer :: typj
            real(kind=8) :: vg(np1,*)
            real(kind=8) :: vgt(np1,*)
            real(kind=8) :: vgt0(np1,*)
            real(kind=8) :: vd(np1,*)
            real(kind=8) :: vd0(np1,*)
            real(kind=8) :: rr(*)
            real(kind=8) :: rr0(*)
            real(kind=8) :: ri(*)
            integer :: ier
            integer :: ichoc
            real(kind=8) :: premac
            real(kind=8) :: prerel
            real(kind=8) :: mtmp1(np1,*)
            real(kind=8) :: mtmp2(np1,*)
            real(kind=8) :: ttr(n2,*)
            real(kind=8) :: u(*)
            real(kind=8) :: w(*)
            real(kind=8) :: d(*)
            integer :: intge1(*)
            integer :: intge2(*)
            integer :: indx(*)
            integer :: indxf(*)
            logical :: loc(*)
          end subroutine calcmd
        end interface
