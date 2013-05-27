        interface
          subroutine ppga1d(ndim,nno,npg,poids,vff,dff,geom,pg)
            integer :: npg
            integer :: nno
            integer :: ndim
            real(kind=8) :: poids(npg)
            real(kind=8) :: vff(nno,npg)
            real(kind=8) :: dff(nno,npg)
            real(kind=8) :: geom(ndim,nno)
            real(kind=8) :: pg(ndim+1,npg)
          end subroutine ppga1d
        end interface
