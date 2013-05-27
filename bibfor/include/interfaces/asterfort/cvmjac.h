        interface
          subroutine cvmjac(mod,nmat,materf,timed,timef,yf,dy,nmod,&
     &epsd,deps,drdy)
            integer :: nmod
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: yf(*)
            real(kind=8) :: dy(*)
            real(kind=8) :: epsd(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: drdy(nmod,nmod)
          end subroutine cvmjac
        end interface
