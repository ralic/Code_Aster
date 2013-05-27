        interface
          subroutine xmcoor(jcesd,jcesv,jcesl,ifiss,ndim,npte,nummae,&
     &ifac,xp,yp,coord)
            integer :: npte
            integer :: jcesd(10)
            integer :: jcesv(10)
            integer :: jcesl(10)
            integer :: ifiss
            integer :: ndim
            integer :: nummae
            integer :: ifac
            real(kind=8) :: xp
            real(kind=8) :: yp
            real(kind=8) :: coord(3)
          end subroutine xmcoor
        end interface
