        interface
          subroutine bmatmc(igau,nbsig,xyz,ipoids,ivf,idfde,nno,nharm,&
     &jacob,b)
            integer :: nbsig
            integer :: igau
            real(kind=8) :: xyz(1)
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            integer :: nno
            real(kind=8) :: nharm
            real(kind=8) :: jacob
            real(kind=8) :: b(nbsig,1)
          end subroutine bmatmc
        end interface
