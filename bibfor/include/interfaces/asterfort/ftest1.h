        interface
          subroutine ftest1(np3,rc,theta,typch,nbseg,xloc,ic,itestc,&
     &toln)
            integer :: np3
            real(kind=8) :: rc(np3,*)
            real(kind=8) :: theta(np3,*)
            integer :: typch(*)
            integer :: nbseg(*)
            real(kind=8) :: xloc(*)
            integer :: ic
            integer :: itestc
            real(kind=8) :: toln
          end subroutine ftest1
        end interface
