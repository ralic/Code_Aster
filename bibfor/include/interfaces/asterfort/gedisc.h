        interface
          subroutine gedisc(ndim,nno,npg,vff,geom,pg)
            integer :: npg
            integer :: nno
            integer :: ndim
            real(kind=8) :: vff(nno,npg)
            real(kind=8) :: geom(ndim,nno)
            real(kind=8) :: pg(ndim+1,npg)
          end subroutine gedisc
        end interface
