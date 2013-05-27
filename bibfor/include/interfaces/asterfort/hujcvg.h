        interface
          subroutine hujcvg(nmat,mater,nvi,vind,vinf,vins,nr,yd,dy,r,&
     &indi,iter,itmax,intg,toler,bnews,mtrac,ye,lreli,iret)
            integer :: nr
            integer :: nvi
            integer :: nmat
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: vind(nvi)
            real(kind=8) :: vinf(nvi)
            real(kind=8) :: vins(nvi)
            real(kind=8) :: yd(nr)
            real(kind=8) :: dy(nr)
            real(kind=8) :: r(nr)
            integer :: indi(7)
            integer :: iter
            integer :: itmax
            integer :: intg
            real(kind=8) :: toler
            logical :: bnews(3)
            logical :: mtrac
            real(kind=8) :: ye(nr)
            logical :: lreli
            integer :: iret
          end subroutine hujcvg
        end interface
