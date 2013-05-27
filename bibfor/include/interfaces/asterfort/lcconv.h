        interface
          subroutine lcconv(loi,yd,dy,ddy,ye,nr,itmax,toler,iter,intg,&
     &nmat,mater,r,rini,epstr,typess,essai,icomp,nvi,vind,vinf,vind1,&
     &indi,bnews,mtrac,lreli,iret)
            integer :: nvi
            integer :: nmat
            integer :: nr
            character(len=16) :: loi
            real(kind=8) :: yd(*)
            real(kind=8) :: dy(*)
            real(kind=8) :: ddy(*)
            real(kind=8) :: ye(nr)
            integer :: itmax
            real(kind=8) :: toler
            integer :: iter
            integer :: intg
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: r(*)
            real(kind=8) :: rini(*)
            real(kind=8) :: epstr(6)
            integer :: typess
            real(kind=8) :: essai
            integer :: icomp
            real(kind=8) :: vind(nvi)
            real(kind=8) :: vinf(nvi)
            real(kind=8) :: vind1(nvi)
            integer :: indi(7)
            logical :: bnews(3)
            logical :: mtrac
            logical :: lreli
            integer :: iret
          end subroutine lcconv
        end interface
