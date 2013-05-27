        interface
          subroutine hayres(mod,nmat,materd,materf,timed,timef,yd,yf,&
     &deps,dy,res,crit,iret)
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: yd(*)
            real(kind=8) :: yf(*)
            real(kind=8) :: deps(6)
            real(kind=8) :: dy(*)
            real(kind=8) :: res(10)
            real(kind=8) :: crit(*)
            integer :: iret
          end subroutine hayres
        end interface
