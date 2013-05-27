        interface
          subroutine disbut(np3,ic,xloc,typobs,xjeu,rayon,theta,nbseg,&
     &cost,sint,dnorm)
            integer :: np3
            integer :: ic
            real(kind=8) :: xloc(*)
            integer :: typobs
            real(kind=8) :: xjeu
            real(kind=8) :: rayon(np3,*)
            real(kind=8) :: theta(np3,*)
            integer :: nbseg
            real(kind=8) :: cost
            real(kind=8) :: sint
            real(kind=8) :: dnorm
          end subroutine disbut
        end interface
