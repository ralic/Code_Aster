        interface
          subroutine xcopco(jcesd,jcesv,jcesl,ifiss,alias,ndim,nummae,&
     &iface,ksi1,ksi2,npte,geom)
            integer :: jcesd(10)
            integer :: jcesv(10)
            integer :: jcesl(10)
            integer :: ifiss
            character(len=8) :: alias
            integer :: ndim
            integer :: nummae
            integer :: iface
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            integer :: npte
            real(kind=8) :: geom(3)
          end subroutine xcopco
        end interface
