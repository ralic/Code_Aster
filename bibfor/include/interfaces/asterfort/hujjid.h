        interface
          subroutine hujjid(mod,mater,indi,deps,prox,proxc,yd,yf,vind,&
     &r,drdy,iret)
            character(len=8) :: mod
            real(kind=8) :: mater(22,2)
            integer :: indi(7)
            real(kind=8) :: deps(6)
            logical :: prox(4)
            logical :: proxc(4)
            real(kind=8) :: yd(18)
            real(kind=8) :: yf(18)
            real(kind=8) :: vind(*)
            real(kind=8) :: r(18)
            real(kind=8) :: drdy(18,18)
            integer :: iret
          end subroutine hujjid
        end interface
