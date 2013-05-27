        interface
          subroutine eps2mc(nno,ndim,nbsig,npg,ipoids,ivf,idfde,xyz,&
     &depl,eps2)
            integer :: nno
            integer :: ndim
            integer :: nbsig
            integer :: npg
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: xyz(1)
            real(kind=8) :: depl(1)
            real(kind=8) :: eps2(1)
          end subroutine eps2mc
        end interface
