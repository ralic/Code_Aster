        interface
          subroutine eps1mc(nno,ndim,nbsig,npg,ipoids,ivf,idfde,xyz,&
     &depl,nharm,eps1)
            integer :: nno
            integer :: ndim
            integer :: nbsig
            integer :: npg
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: xyz(1)
            real(kind=8) :: depl(1)
            real(kind=8) :: nharm
            real(kind=8) :: eps1(1)
          end subroutine eps1mc
        end interface
