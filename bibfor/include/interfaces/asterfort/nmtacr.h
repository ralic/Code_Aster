        interface
          subroutine nmtacr(mode,ndimsi,mat,sigel,vim,epm,dp,sp,xi,f,g&
     &,fds,gds,fdp,gdp,fdx,gdx,dpmax,sig,tang)
            integer :: ndimsi
            integer :: mode
            real(kind=8) :: mat(14)
            real(kind=8) :: sigel(ndimsi)
            real(kind=8) :: vim(9)
            real(kind=8) :: epm(6)
            real(kind=8) :: dp
            real(kind=8) :: sp
            real(kind=8) :: xi
            real(kind=8) :: f
            real(kind=8) :: g
            real(kind=8) :: fds
            real(kind=8) :: gds
            real(kind=8) :: fdp
            real(kind=8) :: gdp
            real(kind=8) :: fdx
            real(kind=8) :: gdx
            real(kind=8) :: dpmax
            real(kind=8) :: sig(ndimsi)
            real(kind=8) :: tang(6,6)
          end subroutine nmtacr
        end interface
