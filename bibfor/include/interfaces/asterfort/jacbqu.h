        interface
          subroutine jacbqu(np3,nbseg,rc,theta,xloc,kn,cn,ic,jacobc,&
     &jacobk)
            integer :: np3
            integer :: nbseg(*)
            real(kind=8) :: rc(np3,*)
            real(kind=8) :: theta(np3,*)
            real(kind=8) :: xloc(*)
            real(kind=8) :: kn
            real(kind=8) :: cn
            integer :: ic
            real(kind=8) :: jacobc(3,*)
            real(kind=8) :: jacobk(3,*)
          end subroutine jacbqu
        end interface
