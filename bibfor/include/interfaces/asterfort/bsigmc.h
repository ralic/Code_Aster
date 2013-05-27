        interface
          subroutine bsigmc(nno,ndim,nbsig,npg,ipoids,ivf,idfde,xyz,&
     &nharm,sigma,bsigma)
            integer :: nno
            integer :: ndim
            integer :: nbsig
            integer :: npg
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            real(kind=8) :: xyz(1)
            real(kind=8) :: nharm
            real(kind=8) :: sigma(1)
            real(kind=8) :: bsigma(1)
          end subroutine bsigmc
        end interface
